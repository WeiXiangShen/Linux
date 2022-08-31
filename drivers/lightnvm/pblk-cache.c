// SPDX-License-Identifier: GPL-2.0
/*
 * Copyright (C) 2016 CNEX Labs
 * Initial release: Javier Gonzalez <javier@cnexlabs.com>
 *                  Matias Bjorling <matias@cnexlabs.com>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License version
 * 2 as published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * pblk-cache.c - pblk's write cache
 */

#include "pblk.h"
#include <linux/sched.h>

void pblk_write_to_cache(struct pblk *pblk, struct bio *bio,
			 unsigned long flags, unsigned int rq_size)
{
	struct pblk_w_ctx w_ctx;
    struct pblk_line_mgmt *l_mg = &pblk->l_mg;
    long *rq_size_mean = l_mg->rq_size_mean;
    int nr_line;

    long mean_50 = *rq_size_mean / 2; // 50% average
    long mean_150 = *rq_size_mean + mean_50; // 150% average
    long s_rq_size = rq_size; // signed variable
    int *rq_amount = l_mg->rq_amount;
    unsigned int *org_rq_size_max = l_mg->org_rq_size_max;
	sector_t lba = pblk_get_lba(bio);
	unsigned long start_time;
	unsigned int bpos, pos;
	int nr_entries = pblk_get_secs(bio);
	int i, ret;
    // float offset;

	start_time = bio_start_io_acct(bio);
	// add by Vynax
/* #ifdef CONFIG_NVM_PBLK_Q_LEARNING
	pblk->i_ino = bio->i_ino;
#endif */
    // decide which line to allocate
    s_rq_size = s_rq_size * PBLK_RQ_MEAN_AMOUNT_MAX * PBLK_RQ_MEAN_AMOUNT_MAX;
    if ( s_rq_size > mean_150 ){
        nr_line = 3;
    }
    else if ( s_rq_size > *rq_size_mean ){
        nr_line = 2;
    }
    else if ( s_rq_size > mean_50 ){
        nr_line = 1;
    }
    else{
        nr_line = 0;
    }

	// add by Vynax
#ifdef CONFIG_NVM_PBLK_Q_LEARNING
    if ( *rq_amount < PBLK_RQ_MEAN_AMOUNT_MAX ){
        (*rq_amount)++;
    }
    // kernel_fpu_begin();
    // offset = bio->bi_iter.bi_size / (*rq_amount);
    // *rq_size_mean += bio->bi_iter.bi_size / *rq_amount;
    
    // printk(KERN_INFO "offset: %f request amount: %d rq_size_mean:%f\n", offset, *rq_amount, *rq_size_mean);

    // kernel_fpu_end();
    *rq_size_mean = *rq_size_mean - *rq_size_mean / *rq_amount;
    *rq_size_mean = *rq_size_mean + s_rq_size / *rq_amount;

    // *rq_size_mean = s_rq_size_mean;

    if ( rq_size > *org_rq_size_max ){
        *org_rq_size_max = rq_size;
    }


    // printk(KERN_INFO "org_rq_size: %u org rq_size_mean: %lu rq_amount: %d nr_line:%d\n", rq_size, *rq_size_mean, *rq_amount, nr_line);
    // printk(KERN_INFO "bi_size: %u org rq_size max: %u rq_size_max: %u\n", bio->bi_iter.bi_size, *org_rq_size_max, *rq_size_max);
    // printk(KERN_INFO "bio->bi_iter.bi_size: %u request amount: %d rq_size_max: %u\n", bio->bi_iter.bi_size, *rq_amount, *rq_size_max);

	// pblk->proc_id = bio->proc_id;
	/* printk(KERN_INFO "bio process id:%u file inode id:%lu\n", task_pid_nr(current),
	       bio->i_ino); */
	/*printk(KERN_INFO "pblk process id:%u file inode id:%lu\n",
	       pblk->proc_id, pblk->i_ino);*/
#endif

	/* Update the write buffer head (mem) with the entries that we can
	 * write. The write in itself cannot fail, so there is no need to
	 * rollback from here on.
	 */
retry:
	ret = pblk_rb_may_write_user(&pblk->rwb, bio, nr_entries, &bpos);
	switch (ret) {
	case NVM_IO_REQUEUE:
		io_schedule();
		goto retry;
	case NVM_IO_ERR:
		pblk_pipeline_stop(pblk);
		bio_io_error(bio);
		goto out;
	}

	pblk_ppa_set_empty(&w_ctx.ppa);
	w_ctx.flags = flags;
	if (bio->bi_opf & REQ_PREFLUSH) {
		w_ctx.flags |= PBLK_FLUSH_ENTRY;
		pblk_write_kick(pblk);
	}

	if (unlikely(!bio_has_data(bio)))
		goto out;

	for (i = 0; i < nr_entries; i++) {
		void *data = bio_data(bio);

		w_ctx.lba = lba + i;
        // add by Vynax
#ifdef CONFIG_NVM_PBLK_Q_LEARNING
		w_ctx.ino_id = bio->i_ino;
        w_ctx.rq_size = nr_entries;
        if ( i == nr_entries - 1 )
            w_ctx.rq_finish = true;
        else
            w_ctx.rq_finish = false;
        w_ctx.nr_line = nr_line;
#endif

		pos = pblk_rb_wrap_pos(&pblk->rwb, bpos + i);
		pblk_rb_write_entry_user(&pblk->rwb, data, w_ctx, pos);

		bio_advance(bio, PBLK_EXPOSED_PAGE_SIZE);
	}

	atomic64_add(nr_entries, &pblk->user_wa);

#ifdef CONFIG_NVM_PBLK_DEBUG
	atomic_long_add(nr_entries, &pblk->inflight_writes);
	atomic_long_add(nr_entries, &pblk->req_writes);
#endif

	pblk_rl_inserted(&pblk->rl, nr_entries);

out:
	bio_end_io_acct(bio, start_time);
	pblk_write_should_kick(pblk);

	if (ret == NVM_IO_DONE)
		bio_endio(bio);
}

/*
 * On GC the incoming lbas are not necessarily sequential. Also, some of the
 * lbas might not be valid entries, which are marked as empty by the GC thread
 */
int pblk_write_gc_to_cache(struct pblk *pblk, struct pblk_gc_rq *gc_rq)
{
	struct pblk_w_ctx w_ctx;
	unsigned int bpos, pos;
	void *data = gc_rq->data;
	int i, valid_entries;

	/* Update the write buffer head (mem) with the entries that we can
	 * write. The write in itself cannot fail, so there is no need to
	 * rollback from here on.
	 */
retry:
	if (!pblk_rb_may_write_gc(&pblk->rwb, gc_rq->secs_to_gc, &bpos)) {
		io_schedule();
		goto retry;
	}

	w_ctx.flags = PBLK_IOTYPE_GC;
	pblk_ppa_set_empty(&w_ctx.ppa);

	for (i = 0, valid_entries = 0; i < gc_rq->nr_secs; i++) {
		if (gc_rq->lba_list[i] == ADDR_EMPTY)
			continue;

		w_ctx.lba = gc_rq->lba_list[i];

		pos = pblk_rb_wrap_pos(&pblk->rwb, bpos + valid_entries);
		pblk_rb_write_entry_gc(&pblk->rwb, data, w_ctx, gc_rq->line,
				       gc_rq->paddr_list[i], pos);

		data += PBLK_EXPOSED_PAGE_SIZE;
		valid_entries++;
	}

	WARN_ONCE(gc_rq->secs_to_gc != valid_entries,
		  "pblk: inconsistent GC write\n");

	atomic64_add(valid_entries, &pblk->gc_wa);

#ifdef CONFIG_NVM_PBLK_DEBUG
	atomic_long_add(valid_entries, &pblk->inflight_writes);
	atomic_long_add(valid_entries, &pblk->recov_gc_writes);
#endif

	pblk_write_should_kick(pblk);
	return NVM_IO_OK;
}
