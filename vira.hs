-- Pipeline configuration for Vira <https://vira.nixos.asia/>

\ctx pipeline ->
  pipeline
    { signoff.enable = False
    , cache.url = Nothing -- Just "https://cache.nixos.asia/oss"
    }
