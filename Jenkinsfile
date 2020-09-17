pipeline {
  agent any

  stages {
    stage('Build & push dependency') {
      when {
        allOf {
          anyOf {
            branch "master"
            branch "sandbox"
          }
          anyOf {
            changeset "Jenkinsfile"
            changeset "Dockerfile*"
            changeset "Makefile"
            changeset "stack.yaml"
            changeset "stack.yaml.lock"
            changeset "**/package.yaml"
          }
        }
      }

      steps {
        sh 'make build-dep'
        sh 'make push-dep'
      }
    }

    stage('Pipeline') {
      when {
        anyOf {
          branch "master"
          branch "sandbox"
          changeRequest()
        }
      }

      stages {
        stage('Docker build') {
          steps {
            sh 'make build -e VERSION=$(git rev-parse --short HEAD)'
          }
        }

        stage('Deployment stage') {
          when {
            anyOf {
              branch "master"
              branch "sandbox"
            }
          }

          stages {
            stage('Docker push') {
              steps {
                sh 'make push -e VERSION=$(git rev-parse --short HEAD)'
              }
            }

            stage('Deploy') {
              environment {
                  BUILD_VERSION="""${sh(
                        returnStdout: true,
                        script: 'git rev-parse --short HEAD'
                    )}"""
                  DEPLOY_VARIANT="${env.BRANCH_NAME}"
              }

              steps {
                sh '''
                  (kubectl create configmap beckn-dhall-config-$DEPLOY_VARIANT \
                    --from-file=deployment-configs/dhall/$DEPLOY_VARIANT \
                    --from-file=deployment-configs/dhall/generic \
                    -n atlas -o yaml --dry-run | \
                    grep -v 'creationTimestamp' \
                  ) > deployment-configs/beckn-dhall-config.yaml
                '''
                kubernetesDeploy(
                      kubeconfigId: 'jenkins-staging-deployer',
                      configs: 'deployment-configs/beckn-dhall-config.yaml',
                      enableConfigSubstitution: true
                    )
                kubernetesDeploy(
                      kubeconfigId: 'jenkins-staging-deployer',
                      configs: 'deployment-configs/*deploy.yaml',
                      enableConfigSubstitution: true
                    )
              }
            }

          }
        }

      }
    }

  }
}
