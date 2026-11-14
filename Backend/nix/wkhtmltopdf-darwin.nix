{ stdenvNoCC, fetchurl, xar, cpio, gzip, gnutar }:
stdenvNoCC.mkDerivation {
  pname = "wkhtmltopdf-bin";
  version = "0.12.6-2";
  src = fetchurl {
    url = "https://github.com/wkhtmltopdf/packaging/releases/download/0.12.6-2/wkhtmltox-0.12.6-2.macos-cocoa.pkg";
    sha256 = "1y4lk0gdbdhicrzs2wrnidb7d0vl0dr14rxapj6xxzh8nmvnp9l1";
  };
  nativeBuildInputs = [ xar cpio gzip gnutar ];
  unpackPhase = ''
    xar -xf $src
    gzip -dc Payload | cpio -id
    mkdir wkhtmltox
    tar xzf usr/local/share/wkhtmltox-installer/wkhtmltox.tar.gz -C wkhtmltox
  '';
  dontBuild = true;
  dontStrip = true;
  dontPatchShebangs = true;
  installPhase = ''
    mkdir -p $out
    cp -r wkhtmltox/bin wkhtmltox/lib wkhtmltox/share $out/
  '';
  meta.platforms = [ "x86_64-darwin" "aarch64-darwin" ];
}
