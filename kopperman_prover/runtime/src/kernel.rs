use cubecl::prelude::*;

// --- 1. HVM POINTER TAGGING ---
// [ Tag (4) | Port (12) | Address (48) ]
const TAG_MASK: u64 = 0xF000_0000_0000_0000;
const PORT_MASK:u64 = 0x0FFF_0000_0000_0000;
const ADDR_MASK:u64 = 0x0000_FFFF_FFFF_FFFF;

#[cube]
struct Link(u64);

#[cube]
fn pack(tag: u32, port: u32, addr: u32) -> Link {
    Link(((tag as u64) << 60) | ((port as u64) << 48) | (addr as u64))
}

#[cube]
fn unpack_addr(l: Link) -> u32 { (l.0 & ADDR_MASK) as u32 }

// --- 2. DATA STRUCTURES ---

#[cube]
struct RedexBag {
    count: Atomic<u32>,
    indices: Array<u32>,
}

// --- 3. WARP-LEVEL PRIMITIVES (The Request) ---
// Optimization: Reduces contention on the 'bag.count' atomic.
// Instead of 32 threads performing 32 atomics, they perform 1 atomic.

#[cube]
fn push_redex_warp(bag: &mut RedexBag, node_idx: u32, is_active: bool) {
    // 1. How many threads in this warp/subgroup have work?
    let active_mask = subgroup_ballot(is_active);
    let total_active = subgroup_count_ones(active_mask);
    let local_rank = subgroup_exclusive_sum(if is_active { 1 } else { 0 });

    // 2. Elect a leader (e.g., first active thread) to reserve space
    let mut base_idx = 0u32;
    if local_rank == 0 && is_active {
        base_idx = Atomic::add(&bag.count, total_active);
    }
    
    // 3. Broadcast base_idx to all threads in warp
    base_idx = subgroup_broadcast_first(base_idx);

    // 4. Write data without conflict
    if is_active {
        bag.indices[base_idx + local_rank] = node_idx;
    }
}

// --- 4. THE INTERACTION KERNEL ---

#[cube(launch)]
pub fn reduce_kernel(
    heap: &mut Array<u64>,
    bag_in: &RedexBag,
    bag_out: &mut RedexBag,
    idx: u32,
) {
    // Bounds check
    if idx >= Atomic::load(&bag_in.count) { return; }

    let addr_a = bag_in.indices[idx];
    
    // HVM Dereference Logic
    let link_b = Link(heap[addr_a]); // Slot 0 is Main
    let addr_b = unpack_addr(link_b);
    
    // Check tags (loaded from metadata slot 3 or encoded)
    // ... Interaction Logic (Annihilation/Commutation) ...

    // Wiring Example:
    // If interaction creates a new redex (Principal <-> Principal),
    // we set flag to true.
    let created_redex = true; // Simplified logic
    let new_node_addr = addr_a; 

    // USE THE WARP OPTIMIZATION
    push_redex_warp(bag_out, new_node_addr, created_redex);
}
