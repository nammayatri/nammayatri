pipeline {
  agent any

  environment {
    DEP_LABEL = "${sh(script: 'echo $JOB_BASE_NAME | sed -e \'s/\\//_/g\' -e \'s/%2F/_/g\' ', returnStdout: true)}"
  }

  stages {
    stage('Build & push dependancy') {
      when {
        anyOf {
          changeset "Jenkinsfile"
          changeset "Dockerfile*"
          changeset "Makefile"
          changeset "stack.yaml"
          changeset "stack.yaml.lock"
          changeset "**/package.yaml"
          expression {
            // build dep on the first run
            return (env.BUILD_NUMBER == "1")
          }
          not {
            changeRequest()
          }
        }
      }

      steps {
        sh 'make build-dep -e DEP_LABEL=$DEP_LABEL'
        sh 'make push-dep -e DEP_LABEL=$DEP_LABEL'
      }
    }

    stage('Pipeline') {

      when {
        anyOf {
          branch "master"
          branch "sandbox"
          changeset "Jenkinsfile"
          changeset "Dockerfile*"
          changeset "Makefile"
          changeset "lib/**/*"
          changeset "app/**/*"
          changeset "stack.yaml"
          changeset "stack.yaml.lock"
          changeset "**/package.yaml"
          changeset "deployment-configs/*deploy.yaml"
          changeset "deployment-configs/**/*.dhall"
          expression {
            // build on the first run
            return (env.BUILD_NUMBER == "1")
          }
        }
      }

      stages {

        stage('Docker build') {
          steps {
            sh 'make build -e VERSION=$(git rev-parse --short HEAD) -e DEP_LABEL=$DEP_LABEL'
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
