pipeline {
    agent {
        label 'nixos'
    }
    stages {
        stage ('Matrix') {
            matrix {
                agent {
                    label "${SYSTEM}"
                }
                axes {
                    axis {
                        name 'SYSTEM'
                        // Disabled until we figure out a way to build these only on 'main'
                        // values 'x86_64-linux', 'aarch64-darwin', 'x86_64-darwin'
                        values 'x86_64-linux'
                    }
                }
                stages {
                    stage ('Cachix setup') {
                        steps {
                            cachixUse 'nammayatri'
                        }
                    }
                    stage ('Nix Build All') {
                        steps {
                            nixBuildAll system: env.SYSTEM
                        }
                    }
                    stage ('Docker image') {
                        when {
                            allOf {
                                expression { 'x86_64-linux' == env.SYSTEM }
                                anyOf {
                                    branch 'main'; branch 'prodHotPush'
                                }
                            }
                        }
                        steps {
                            dockerPush "dockerImage", "ghcr.io"
                        }
                    }
                    stage ('Cachix push') {
                        when {
                            anyOf {
                                branch 'main'; branch 'prodHotPush'
                            }
                        }
                        steps {
                            cachixPush "nammayatri"
                        }
                    }
                }
            }
        }
    }
}
