pipeline {
    agent none
    stages {
        stage ('Matrix') {
            matrix {
                agent {
                    label "${SYSTEM}"
                }
                when {
                    anyOf {
                        expression { 'x86_64-linux' == env.SYSTEM }
                        // Enable running macOS / Linux ARM builds when on main
                        // branch, so as to provide Nix cache for people on
                        // macOS.
                        branch 'main'
                        // TODO: remove this
                        branch 'linux-arm'
                    }
                }
                axes {
                    axis {
                        name 'SYSTEM'
                        values 'x86_64-linux', 'aarch64-linux' // , 'aarch64-darwin', 'x86_64-darwin'
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
                            nixCI system: env.SYSTEM
                        }
                    }
                    stage ('Docker image') {
                        when {
                            allOf {
                                expression { 'x86_64-linux' == env.SYSTEM }
                                anyOf {
                                    branch 'main'; branch 'prodHotPush';
                                    changeRequest target: 'main'
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
