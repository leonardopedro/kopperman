mod kernel;
mod loader;
mod verifier;
#[path = "../protocol_generated.rs"] mod protocol_generated;

use cubecl::prelude::*;

#[tokio::main]
async fn main() {
    // 1. Load Data
    let bin = std::fs::read("../runtime/proof.bin").unwrap();
    let initial_heap = loader::load_flatbuffer(&bin);

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
