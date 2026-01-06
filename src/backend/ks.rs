use std::collections::HashSet;

use crate::mir::{Block, BlockId, Body, Terminator};

fn preds_succs(blocks: &[Block]) -> (Vec<Vec<BlockId>>, Vec<Vec<BlockId>>) {
    let mut preds = vec![vec![]; blocks.len()];
    let mut succs = vec![vec![]; blocks.len()];

    for b in 0..blocks.len() {
        let term = &blocks[b].terminator;  
        let id = BlockId(b as u32); 
        match term {
            Terminator::Goto(next) => {
                preds[next.0 as usize].push(id);
                succs[b].push(*next);
            }
            Terminator::Branch(_, tbb, ebb) => {
                preds[tbb.0 as usize].push(id);
                preds[ebb.0 as usize].push(id);
                succs[b].push(*tbb);
                succs[b].push(*ebb);
            }
            Terminator::Return(_) => {},
            Terminator::Unreachable => {},
        }
    }

    (preds, succs)
}

fn find_doms(blocks: &[Block], preds: &[Vec<BlockId>]) -> Vec<HashSet<BlockId>> {
    let all: HashSet<BlockId> = (0..blocks.len() as u32).map(BlockId).collect();
    let mut dom = vec![all.clone(); blocks.len()];
    dom[0].clear();
    dom[0].insert(BlockId(0));
    dom[0].shrink_to_fit();

    let mut changed = true;
    while changed {
        changed = false;

        for b in 1..blocks.len() {
            let mut new = all.clone();

            for &p in &preds[b] {
                new = new.intersection(&dom[p.0 as usize]).cloned().collect();
            }

            new.insert(BlockId(b as u32));

            if new != dom[b] {
                dom[b] = new;
                changed = true;
            }
        }
    }

    dom
}

fn dominates(dom: &[HashSet<BlockId>], a: BlockId, b: BlockId) -> bool {
    dom[b.0 as usize].contains(&a)
}

fn is_back_edge(dom: &[HashSet<BlockId>], from: BlockId, to: BlockId) -> bool {
    dominates(dom, to, from)
}

pub fn print_doms(body: &Body) {
    let (preds, succs) = preds_succs(&body.blocks);
    let doms = find_doms(&body.blocks, &preds);
    for b in 0..body.blocks.len() {
        println!("{{ {} }} -> block{} -> {{ {} }}", preds[b].iter().map(|id| format!("block{}", id.0)).collect::<Vec<_>>().join(", "), b, succs[b].iter().map(|id| format!("block{}", id.0)).collect::<Vec<_>>().join(", "));
    }
    for b in 0..body.blocks.len() {
        println!("dom(block{}) = {{ {} }}", b, doms[b].iter().map(|id| format!("block{}", id.0)).collect::<Vec<_>>().join(", "));
    }
}
