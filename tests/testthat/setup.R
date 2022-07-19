
failed_test_files_path <- file.path(
    tools::file_path_as_absolute("../"),
    "failed_test_files"
)
print(failed_test_files_path)
unlink(failed_test_files_path, recursive = TRUE)
traceback()
