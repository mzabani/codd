import "${builtins.fetchGit {
  url = "https://github.com/mzabani/codd.git";
  ref = "master";
  submodules = true;
}}" { }
