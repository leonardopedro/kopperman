use crate::protocol_generated::protocol::{get_root_as_graph, Tag};

pub fn load_flatbuffer(data: &[u8]) -> Vec<u64> {
    let graph = get_root_as_graph(data);
    let nodes = graph.nodes().unwrap();
    
    // 4 u64s per node (HVM Layout)
    let mut heap = vec![0u64; nodes.len() * 4];
    
    for (i, node) in nodes.iter().enumerate() {
        let base = i * 4;
        let pack = |tag, port, addr| {
            ((tag as u64) << 60) | ((port as u64) << 48) | (addr as u64)
        };
        
        // Main Port
        if let Some(m) = node.main() {
            heap[base] = pack(m.tag().0, m.port(), m.addr());
        }
        // ... Aux ports and Metadata ...
    }
    heap
}
