pipeline {
    agent any
    stages {
        stage ('Cachix setup') {
            steps {
                sh 'cachix use nammayatri'
            }
        }
        stage ('Nix Build') {
            environment {
                CACHIX_AUTH_TOKEN = credentials('cachix-auth-token')
            }
            steps {
                nixBuildAndCachixPush '.#nammayatri'
                nixBuildAndCachixPush '.#devShells.x86_64-linux.default'
            }
        }
        stage ('Flake check') {
            steps {
                sh 'nix build -L .#check'
            }
        }
        stage ('Build docker image') {
            steps {
                sh 'nix build .#dockerImage -o dockerImage.tgz'
                sh 'nix eval --raw .#dockerImageName > ./dockerImageName'
                stash includes: 'dockerImage*', name: 'dockerImage'
            }
        }
        stage ('Docker image') {
            when { branch 'main' }
            environment {
              DOCKER_USER = credentials('docker-user')
              DOCKER_PASS = credentials('docker-pass')
            }
            steps {
                unstash 'dockerImage'
                sh 'docker load -i ./dockerImage.tgz'
                sh '''
                   IMAGE_NAME=$(cat ./dockerImageName)
                   echo ${DOCKER_PASS} | docker login -u ${DOCKER_USER} --password-stdin ghcr.io
                   docker push ${IMAGE_NAME}
                   docker logout ghcr.io
                   '''
            }
        }
    }
}

// Run a nix command such that new paths created in the nix store are
// automatically pushed to cachix. 
//
// We limit this to 'nix build' because other commands, like 'nix develop' may
// run arbitrary commands by PR authors in the context of CI environment.
//
// Ref: https://docs.cachix.org/pushing#push-all-store-paths-produced-during-a-command
def nixBuildAndCachixPush(String args) {
    sh "cachix watch-exec nammayatri -- nix build -L ${args}"
}
