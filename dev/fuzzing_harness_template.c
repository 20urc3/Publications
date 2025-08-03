/// Fuzzing Harness template
/// Written by Marc Van Hausser of SRlabs
/// Link to the article: https://www.srlabs.de/blog-post/unlocking-secrets-effective-fuzzing-harness

int LLVMFuzzerInitialize(int argc, char **argv) {
  // [Optional] One-time initialization goes here.
  // e.g., open files, set up constant global resources.
  // This runs once before fuzzing starts to prevent repetition on each input.
  return 0;
}

int LLVMFuzzerTestOneInput(char *data, size_t len) {
  if (len < XXX) return 0;  
  // 1. Minimum length check: replace XXX with required size.
  //
  // 2. [Optional] Transform or interpret data into parameters.
  //    - Use FuzzedDataProvider or manual parsing to prevent data reuse.
  //    - Do not reinterpret the same bytes in incompatible ways.
  //    e.g., int param = data[0]; string payload = std::string(data+1, len-1);
  //
  // 3. [Optional] Set up the target state or environment.
  //    - If the target requires some initialization for each run, do it here.
  //    - Keep it minimal and avoid persistent state across runs.
  //
  // 4. Call the target API/function with the prepared data.
  //    e.g., result = target_function(param, payload);
  //
  // 5. [Optional] Verify outputs or post-conditions.
  //    - If target returns a struct or data, validate pointers or fields.
  //    - Perform invariant checks to ensure the target's state is consistent.
  //    - If something is wrong, abort() to indicate a crash.
  //
  // 6. [Optional] Clean up.
  //    - Free allocated memory and reset modified global state.
  //    - Undo anything before the next run.
  //
  // Note: During fuzzing, avoid debug prints or file I/O in this function!
  // If an input should be discarded (not added to corpus), return -1.
  return 0;
}
