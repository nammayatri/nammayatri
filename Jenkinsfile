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
                        // Enable running macOS / Linux ARM builds when on 'main' or PRs
                        // - macOS: Provide Nix cache to devs using macOS
                        // - Linux ARM: For AWS graviton deployment
                        branch 'main'
                    }
                }
                axes {
                    axis {
                        name 'SYSTEM'
                        values 'x86_64-linux', 'aarch64-darwin', 'x86_64-darwin' // 'aarch64-linux'
                    }
                }
                stages {
                    stage ('Cachix setup') {
                        steps {
                            cachixUse 'nammayatri'
                        }
                    }
                    stage ('nix eval') {
                        steps {
                            sh "nix --version"
                            sh "nix eval .#nixci.default --json"
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
                                // TODO: Build for aarch64-linux
                                // Requires https://github.com/juspay/jenkins-nix-ci/issues/32
                                expression { 'x86_64-linux' == env.SYSTEM }
                                anyOf {
                                    branch 'main'; branch 'prodHotPush-Common'; branch 'prodHotPush-BAP'; branch 'prodHotPush-BPP'; branch 'prodHotPush-Schedulers';
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
                                branch 'main'; branch 'prodHotPush-Common'; branch 'prodHotPush-BAP'; branch 'prodHotPush-BPP'; branch 'prodHotPush-Schedulers';
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
