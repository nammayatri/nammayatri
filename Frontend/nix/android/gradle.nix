{ callPackage, gradleGen, jdk11 }:

callPackage
  (gradleGen {
    version = "8.0";
    nativeVersion = "0.22-milestone-24";
    sha256 = "124pny4kmy74bma251j3ylqhvvf7yfl2mx83rs486jkkxhwbjna1";
    defaultJava = jdk11;
  })
{ }
