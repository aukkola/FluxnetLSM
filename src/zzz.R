#!/usr/bin/env Rscript

# Get git revision
git_rev <- system("git rev-parse --verify HEAD", intern = TRUE)

if (length(git_rev) == 0) {
    # Not a git repository!
    git_rev <- "NON-GIT"
} else {
    porcelain <- (system("git status --porcelain", intern = TRUE))
    if (length(porcelain) > 0) {
        git_rev <- paste0(git_rev, "-dirty")
    }
}

message("git rev: ", git_rev)

desc <- read.dcf("../DESCRIPTION")
if ("git_revision" %in% colnames(desc)) {
    desc[1, "git_revision"] <- git_rev
} else {
    update <- matrix(git_rev, dimnames = list(NULL, "git_revision"))
    desc <- cbind(desc, update)
}
write.dcf(desc, "../DESCRIPTION")
