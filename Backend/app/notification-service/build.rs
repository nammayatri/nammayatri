fn main() {
    #[allow(clippy::expect_used)]
    tonic_build::compile_protos("protos/notification.proto")
        .expect("Failed to compile `notification.proto` file");
}
