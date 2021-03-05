#

.create_if_not_exists <- function(path) {
  if (!dir.exists(path)) {
    dir.create(path)
  }
}

# Create folders
.create_if_not_exists(file.path("../output"))
.create_if_not_exists(file.path("../plots"))
.create_if_not_exists(file.path("../plots/outliers"))

# TODO: Create folders in the correct file where needed
