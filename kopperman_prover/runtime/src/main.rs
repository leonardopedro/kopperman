mod kernel;
mod loader;
mod verifier;
mod collab;
#[path = "../protocol_generated.rs"] mod protocol_generated;

use cubecl::prelude::*;
use loro::LoroDoc;

#[tokio::main]
async fn main() {
    // 1. Load Collaborative State or Binary
    let initial_heap = if std::path::Path::new("proof.loro").exists() {
        println!("Loading from proof.loro...");
        let bytes = std::fs::read("proof.loro").unwrap();
        let doc = LoroDoc::decode(&bytes).unwrap();
        let proof_json = collab::export_proof(&doc);
        
        // In a real scenario, you'd send this JSON back through Dhall 
        // to get the final optimized FlatBuffer. 
        // For now, we assume we have the compiled proof.bin.
        let bin = std::fs::read("../runtime/proof.bin").expect("proof.bin not found. Run compiler first.");
        loader::load_flatbuffer(&bin)
    } else {
        let bin = std::fs::read("../runtime/proof.bin").expect("proof.bin not found. Run compiler first.");
        loader::load_flatbuffer(&bin)
    };

    // 2. Setup GPU
    let client = CubeClient::new(CubeClientOptions::default());
    let mut heap = client.create_buffer_from_data(&initial_heap);
    
    // Ping-Pong Bags
    let mut bag_a = client.create_buffer(1024 * 1024); // Size u32
    let mut bag_b = client.create_buffer(1024 * 1024);
    
    // 3. Run Loop
    for i in 0..1000 {
        kernel::reduce_kernel::launch(
            &client, 
            CubeLaunchOptions::default(), 
            &mut heap, 
            &bag_a, 
            &mut bag_b
        );
        // Swap bags logic...
    }

    // 4. Verify
    let res = heap.read().await;
    verifier::verify(bytemuck::cast_slice(&res));
}
