.First <- function(){
  root = rappdirs::user_data_dir("columbine")
  if (!dir.exists(root)) {
    ok <- dir.create(root, recursive = TRUE)
    if (!ok) stop("unable tp create root data directory:", root)
  }
}