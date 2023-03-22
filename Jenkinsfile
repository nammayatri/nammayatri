pipeline {
    agent any
    stages {
        stage ('Cachix setup') {
            steps {
                sh 'nix run nixpkgs#cachix use nammayatri'
            }
        }
        stage ('Nix Build') {
            steps {
                sh 'nix build -L .#nammayatri'
            }
        }
        stage ('Flake check') {
            steps {
                sh 'nix build -L .#check'
            }
        }
        stage ('Build docker image') {
            steps {
                sh 'docker load -i $(nix build .#dockerImage --print-out-paths)'
                sh 'echo "$(nix eval --raw .#packages.x86_64-linux.dockerImage.buildArgs.name):$(nix eval --raw .#packages.x86_64-linux.dockerImage.buildArgs.tag)" > ./dockerImageName'
                stash includes: 'dockerImageName', name: 'dockerImageName'
            }
        }
        stage ('Docker image') {
            when { branch 'main' }
            environment {
              DOCKER_USER = credentials('docker-user')
              DOCKER_PASS = credentials('docker-pass')
            }
            steps {
                unstash 'dockerImageName'
                sh '''
                   IMAGE_NAME=$(cat ./dockerImageName | tr -cd "'[:alnum:]/.:")
                   echo ${DOCKER_PASS} | docker login -u ${DOCKER_USER} --password-stdin ghcr.io
                   docker push "${IMAGE_NAME}"
                   docker logout ghcr.io
                   '''
            }
        }
        /* stage ('Push to cachix') {
          environment {
            CACHIX_AUTH_TOKEN = credentials('cachix-auth-token')
          }
          steps {
            sh 'nix run .#cachix-push'
          }
        } */
    }
}
