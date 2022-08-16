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
 * pblk-map.c - pblk's lba-ppa mapping strategy
 *
 */

#include "pblk.h"

static int pblk_map_page_data(struct pblk *pblk, unsigned int sentry,
			      struct ppa_addr *ppa_list,
			      unsigned long *lun_bitmap,
			      void *meta_list,
			      unsigned int valid_secs)
{
	struct pblk_line *line[PBLK_OPEN_LINE];
    // line[0] = pblk_line_get_data(pblk);
	struct pblk_emeta *emeta[PBLK_OPEN_LINE];
	struct pblk_w_ctx *w_ctx;
	__le64 *lba_list[PBLK_OPEN_LINE];
	u64 paddr[PBLK_OPEN_LINE];
	int nr_secs = pblk->min_write_pgs;
	int i;
    // add by Vynax
#ifdef CONFIG_NVM_PBLK_Q_LEARNING
    // struct pblk_q_learning *q_learn = &pblk->q_learn;
    int nr_line[PBLK_OPEN_LINE];
    int count_line[PBLK_OPEN_LINE]; // count how many entry has been allocated to this line
    int w_l; // which line in entry
    int blank_secs_each_line = (nr_secs - valid_secs) / PBLK_OPEN_LINE; // (nr_secs - valid_secs) / PBLK_OPEN_LINE
    int blank_secs_else = (nr_secs - valid_secs) % PBLK_OPEN_LINE; // (nr_secs - valid_secs) % PBLK_OPEN_LINE
    int BENL = 0; //blank entry now line;
    memset(nr_line, 0, sizeof(nr_line));
    for (i = 0; i < nr_secs; i++) {
        if (i < valid_secs) {
            w_ctx = pblk_rb_w_ctx(&pblk->rwb, sentry + i);
            nr_line[w_ctx->nr_line]++;
        }
    }

    for (i = 0; i < PBLK_OPEN_LINE; i++) {
        nr_line[i]+= blank_secs_each_line;
        if ( blank_secs_else != 0 ){
            nr_line[i] ++;
            blank_secs_else --;
        }
    }
    // nr_line[0] += valid_secs;
    // printk(KERN_INFO "nr_line[0]:%d [1]:%d [2]:%d [3]:%d valid_secs:%u\n", nr_line[0], nr_line[1], nr_line[2], nr_line[3], valid_secs);
    printk(KERN_INFO "nr_line[0]: %d [1]:%d valid_secs:%u\n", nr_line[0], nr_line[1], valid_secs);
    // printk(KERN_INFO "nr_line[0]: %d valid_secs:%u nr_secs:%d\n", nr_line[0], valid_secs, nr_secs);
#endif
    // printk(KERN_INFO "pblk_map_page_data start\n");

    for ( i=0;i< PBLK_OPEN_LINE; i++){
        line[i] = pblk->l_mg.data_line[i];
        if (!line[i])
            return -ENOSPC;

        // printk(KERN_INFO "line check completed\n");
        if (pblk_line_is_full(line[i])) {
            struct pblk_line *prev_line = line[i];

            /* If we cannot allocate a new line, make sure to store metadata
            * on current line and then fail
            */
            line[i] = pblk_line_replace_data(pblk);
            pblk_line_close_meta(pblk, prev_line);

            if (!line[i]) {
                pblk_pipeline_stop(pblk);
                return -ENOSPC;
            }

        }
    }

    // line[0] = pblk_line_get_data(pblk);

    // printk(KERN_INFO "line full check completed\n");

	emeta[0] = line[0]->emeta;
	lba_list[0] = emeta_to_lbas(pblk, emeta[0]->buf);

	paddr[0] = pblk_alloc_page(pblk, line[0], nr_line[0]);

    emeta[1] = line[1]->emeta;
	lba_list[1] = emeta_to_lbas(pblk, emeta[1]->buf);

	paddr[1] = pblk_alloc_page(pblk, line[1], nr_line[1]);

	for (i = 0; i < valid_secs; i++) {
		struct pblk_sec_meta *meta = pblk_get_meta(pblk, meta_list, i);
		__le64 addr_empty = cpu_to_le64(ADDR_EMPTY);

        w_ctx = pblk_rb_w_ctx(&pblk->rwb, sentry + i);
        w_l = w_ctx->nr_line;

		/* ppa to be sent to the device */
		ppa_list[i] = addr_to_gen_ppa(pblk, paddr[w_l], line[w_l]->id);

		/* Write context for target bio completion on write buffer. Note
		 * that the write buffer is protected by the sync backpointer,
		 * and a single writer thread have access to each specific entry
		 * at a time. Thus, it is safe to modify the context for the
		 * entry we are setting up for submission without taking any
		 * lock or memory barrier.
		 */
		if (i < valid_secs) {
			kref_get(&line[w_l]->ref);
			atomic_inc(&line[w_l]->sec_to_update);
			// w_ctx = pblk_rb_w_ctx(&pblk->rwb, sentry + i);
			w_ctx->ppa = ppa_list[i];
			meta->lba = cpu_to_le64(w_ctx->lba);
			lba_list[w_l][paddr[w_l]] = cpu_to_le64(w_ctx->lba);
			if (lba_list[w_l][paddr[w_l]] != addr_empty)
				line[w_l]->nr_valid_lbas++;
			else
				atomic64_inc(&pblk->pad_wa);
		} 
        /* else {
			lba_list[0][paddr[0]] = addr_empty;
			meta->lba = addr_empty;
			__pblk_map_invalidate(pblk, line[0], paddr[0]);
		} */

        // add by Vynax
#ifdef CONFIG_NVM_PBLK_Q_LEARNING
        if ( i< 4 )
        {
            // int pblk_lba_bucket_upper_limit = PBLK_LBA_BUCKET - 1;
            // q_learn->q_table[ i * i * i * i * i ]++;
            // printk(KERN_INFO "q_table[%d][%d][%d][%d][%d]:%u\n", i, i, i, i, i, q_learn->q_table[ i * i * i * i * i ]);
        }
        // printk(KERN_INFO "w_ctx : nr_line %u", w_ctx->nr_line);
        paddr[w_l]++;
#endif

	}

    for (i = valid_secs; i < nr_secs; i++ ) {
		struct pblk_sec_meta *meta = pblk_get_meta(pblk, meta_list, i);
		__le64 addr_empty = cpu_to_le64(ADDR_EMPTY);

        // w_ctx = pblk_rb_w_ctx(&pblk->rwb, sentry + i);
        // w_l = w_ctx->nr_line;

		// ppa to be sent to the device
		ppa_list[i] = addr_to_gen_ppa(pblk, paddr[BENL], line[BENL]->id);
        lba_list[BENL][paddr[BENL]] = addr_empty;
		meta->lba = addr_empty;
		__pblk_map_invalidate(pblk, line[BENL], paddr[BENL]);

        paddr[BENL]++;
        BENL = (BENL + 1) % PBLK_OPEN_LINE;

        printk(KERN_INFO "blank entry WTF oh Yeah!\n");
    }
    
    // my_pblk_map_invalidate(pblk, line);

	pblk_down_rq(pblk, ppa_list[0], lun_bitmap);
	return 0;
}

static int pblk_map_page_data_alloc(struct pblk *pblk, unsigned int sentry,
			      struct ppa_addr *ppa_list,
			      unsigned long *lun_bitmap,
			      void *meta_list,
			      unsigned int valid_secs){
    struct pblk_line *line = pblk_line_get_data(pblk);
	struct pblk_emeta *emeta;
	struct pblk_w_ctx *w_ctx;
	__le64 *lba_list;
	u64 paddr;
	int nr_secs = pblk->min_write_pgs;
	int i;

    emeta = line->emeta;
	lba_list = emeta_to_lbas(pblk, emeta->buf);

	paddr = pblk_alloc_page(pblk, line, nr_secs);

	for (i = 0; i < nr_secs; i++, paddr++) {
		struct pblk_sec_meta *meta = pblk_get_meta(pblk, meta_list, i);
		__le64 addr_empty = cpu_to_le64(ADDR_EMPTY);

		/* ppa to be sent to the device */
		ppa_list[i] = addr_to_gen_ppa(pblk, paddr, line->id);

		/* Write context for target bio completion on write buffer. Note
		 * that the write buffer is protected by the sync backpointer,
		 * and a single writer thread have access to each specific entry
		 * at a time. Thus, it is safe to modify the context for the
		 * entry we are setting up for submission without taking any
		 * lock or memory barrier.
		 */
		if (i < valid_secs) {
			kref_get(&line->ref);
			atomic_inc(&line->sec_to_update);
			w_ctx = pblk_rb_w_ctx(&pblk->rwb, sentry + i);
			w_ctx->ppa = ppa_list[i];
			meta->lba = cpu_to_le64(w_ctx->lba);
			lba_list[paddr] = cpu_to_le64(w_ctx->lba);
			if (lba_list[paddr] != addr_empty)
				line->nr_valid_lbas++;
			else
				atomic64_inc(&pblk->pad_wa);
		} else {
			lba_list[paddr] = addr_empty;
			meta->lba = addr_empty;
			__pblk_map_invalidate(pblk, line, paddr);
		}
    }

    return 0;
}

static int pblk_map_page_data_check(struct pblk *pblk){
    struct pblk_line_mgmt *l_mg = &pblk->l_mg;
    struct pblk_line *line;
    int i;
    for (i = 1; i < PBLK_OPEN_LINE; i++) {
        line = l_mg->data_line[i];
        if (!line)
		    return -ENOSPC;

        // printk(KERN_INFO "line check completed\n");
        if (pblk_line_is_full(line)) {
            struct pblk_line *prev_line = line;

            /* If we cannot allocate a new line, make sure to store metadata
            * on current line and then fail
            */
            line = pblk_line_replace_data(pblk);
            pblk_line_close_meta(pblk, prev_line);

            if (!line) {
                pblk_pipeline_stop(pblk);
                return -ENOSPC;
            }

        }
    }
}

int pblk_map_rq(struct pblk *pblk, struct nvm_rq *rqd, unsigned int sentry,
		 unsigned long *lun_bitmap, unsigned int valid_secs,
		 unsigned int off)
{
	void *meta_list = pblk_get_meta_for_writes(pblk, rqd);
	void *meta_buffer;
	struct ppa_addr *ppa_list = nvm_rq_to_ppa_list(rqd);
	unsigned int map_secs;
	int min = pblk->min_write_pgs;
	int i;
	int ret;

	for (i = off; i < rqd->nr_ppas; i += min) {
		map_secs = (i + min > valid_secs) ? (valid_secs % min) : min;
		meta_buffer = pblk_get_meta(pblk, meta_list, i);

		ret = pblk_map_page_data(pblk, sentry + i, &ppa_list[i],
					lun_bitmap, meta_buffer, map_secs);
		if (ret)
			return ret;
	}

	return 0;
}

/* only if erase_ppa is set, acquire erase semaphore */
int pblk_map_erase_rq(struct pblk *pblk, struct nvm_rq *rqd,
		       unsigned int sentry, unsigned long *lun_bitmap,
		       unsigned int valid_secs, struct ppa_addr *erase_ppa)
{
	struct nvm_tgt_dev *dev = pblk->dev;
	struct nvm_geo *geo = &dev->geo;
	struct pblk_line_meta *lm = &pblk->lm;
	void *meta_list = pblk_get_meta_for_writes(pblk, rqd);
	void *meta_buffer;
	struct ppa_addr *ppa_list = nvm_rq_to_ppa_list(rqd);
	struct pblk_line *e_line, *d_line;
	unsigned int map_secs;
	int min = pblk->min_write_pgs;
	int i, erase_lun;
	int ret;


	for (i = 0; i < rqd->nr_ppas; i += min) {
		map_secs = (i + min > valid_secs) ? (valid_secs % min) : min;
		meta_buffer = pblk_get_meta(pblk, meta_list, i);

		ret = pblk_map_page_data(pblk, sentry + i, &ppa_list[i],
					lun_bitmap, meta_buffer, map_secs);
		if (ret)
			return ret;

		erase_lun = pblk_ppa_to_pos(geo, ppa_list[i]);

		/* line can change after page map. We might also be writing the
		 * last line.
		 */
		e_line = pblk_line_get_erase(pblk);
		if (!e_line)
			return pblk_map_rq(pblk, rqd, sentry, lun_bitmap,
							valid_secs, i + min);

		spin_lock(&e_line->lock);
		if (!test_bit(erase_lun, e_line->erase_bitmap)) {
			set_bit(erase_lun, e_line->erase_bitmap);
			atomic_dec(&e_line->left_eblks);

			*erase_ppa = ppa_list[i];
			erase_ppa->a.blk = e_line->id;
			erase_ppa->a.reserved = 0;

			spin_unlock(&e_line->lock);

			/* Avoid evaluating e_line->left_eblks */
			return pblk_map_rq(pblk, rqd, sentry, lun_bitmap,
							valid_secs, i + min);
		}
		spin_unlock(&e_line->lock);
	}

	d_line = pblk_line_get_data(pblk);

	/* line can change after page map. We might also be writing the
	 * last line.
	 */
	e_line = pblk_line_get_erase(pblk);
	if (!e_line)
		return -ENOSPC;

	/* Erase blocks that are bad in this line but might not be in next */
	if (unlikely(pblk_ppa_empty(*erase_ppa)) &&
			bitmap_weight(d_line->blk_bitmap, lm->blk_per_line)) {
		int bit = -1;

retry:
		bit = find_next_bit(d_line->blk_bitmap,
						lm->blk_per_line, bit + 1);
		if (bit >= lm->blk_per_line)
			return 0;

		spin_lock(&e_line->lock);
		if (test_bit(bit, e_line->erase_bitmap)) {
			spin_unlock(&e_line->lock);
			goto retry;
		}
		spin_unlock(&e_line->lock);

		set_bit(bit, e_line->erase_bitmap);
		atomic_dec(&e_line->left_eblks);
		*erase_ppa = pblk->luns[bit].bppa; /* set ch and lun */
		erase_ppa->a.blk = e_line->id;
	}

	return 0;
}
