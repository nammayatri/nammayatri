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
                sh 'nix build .#dockerImage -o dockerImage.tgz'
                sh 'nix eval --raw .#dockerImageName > ./dockerImageName'
                stash includes: 'dockerImage*', name: 'dockerImage'
            }
        }
        stage ('Docker image') {
            when { changeRequest branch: 'nixify' }
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
                   '''
            }
        }
        stage ('Push to cachix') {
          environment {
            CACHIX_AUTH_TOKEN = credentials('cachix-auth-token')
          }
          steps {
            sh 'nix run .#cachix-push'
          }
        }
    }
}
