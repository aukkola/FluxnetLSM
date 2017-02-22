
git_rev <- system("git rev-parse --verify HEAD", intern = TRUE)

message('git rev: ', git_rev)

desc <- read.dcf("../DESCRIPTION")

update <- matrix(git_rev, dimnames = list(NULL, "git_revision"))

desc <- cbind(desc, update)

write.dcf(desc, "../DESCRIPTION")
