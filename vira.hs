-- Pipeline configuration for Vira <https://vira.nixos.asia/>

\ctx pipeline ->
  pipeline
    { signoff.enable = False
    , cache.url = Just "https://cache.nixos.asia/oss"
    }
