{ callPackage, gradleGen, jdk11 }:

callPackage
  (gradleGen {
    version = "8.0.1";
    nativeVersion = "0.22-milestone-24";
    sha256 = "02g9i1mrpdydj8d6395cv6a4ny9fw3z7sjzr7n6l6a9zx65masqv";
    defaultJava = jdk11;
  })
{ }
