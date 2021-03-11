pipeline {
  agent any

  environment {
      BUILD_VERSION="""${sh(
            returnStdout: true,
            script: 'git rev-parse --short HEAD'
        )}"""
      DEPLOY_VARIANT="${env.BRANCH_NAME}"
  }

  stages {
    stage('Build & push dependency') {
      when {
        allOf {
          anyOf {
            branch "master"
            branch "sandbox"
            branch "production"
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
        sh 'make aws-auth && \
            make push-dep -e VERSION=$(git rev-parse --short HEAD) -e NS="147728078333.dkr.ecr.ap-south-1.amazonaws.com"'
      }
    }

    stage('Pipeline') {
      when {
        anyOf {
          branch "master"
          branch "sandbox"
          branch "production"
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
              branch "production"
            }
          }

          stages {

            stage('Docker push') {
              steps {
                sh 'make push -e VERSION=$(git rev-parse --short HEAD)'
                sh 'make aws-auth && \
                    make push -e VERSION=$(git rev-parse --short HEAD) -e NS="147728078333.dkr.ecr.ap-south-1.amazonaws.com"'
              }
            }

            stage('Apply configs') {
              when {
                anyOf {
                  branch "master"
                  branch "sandbox"
                }
              }

              steps {
                sh '''
                  (kubectl create configmap beckn-dhall-config-$DEPLOY_VARIANT \
                    --from-file=dhall-configs/$DEPLOY_VARIANT \
                    --from-file=globalCommon.dhall=dhall-configs/generic/common.dhall \
                    -n atlas -o yaml --dry-run | \
                    grep -v 'creationTimestamp' \
                  ) > deployment-configs/beckn-dhall-config.yaml
                '''
                kubernetesDeploy(
                      kubeconfigId: 'eks-user-staging',
                      configs: 'deployment-configs/beckn-dhall-config.yaml',
                      enableConfigSubstitution: true
                    )
                kubernetesDeploy(
                      kubeconfigId: 'eks-user-staging',
                      configs: 'deployment-configs/*deploy.yaml',
                      enableConfigSubstitution: true
                    )
              }
            }

            stage('Deploy master') {
              when { branch "master" }
              steps {
                kubernetesDeploy(
                      kubeconfigId: 'eks-user-staging',
                      configs: 'deployment-configs/*deploy.yaml',
                      enableConfigSubstitution: true
                    )
              }
            }

            stage('Deploy sandbox') {
              when { branch "sandbox" }
              steps {
                kubernetesDeploy(
                      kubeconfigId: 'eks-user-staging',
                      configs: 'deployment-configs/app-backend-deploy.yaml',
                      enableConfigSubstitution: true
                    )
                kubernetesDeploy(
                      kubeconfigId: 'eks-user-staging',
                      configs: 'deployment-configs/beckn-gateway-deploy.yaml',
                      enableConfigSubstitution: true
                    )
                kubernetesDeploy(
                      kubeconfigId: 'eks-user-staging',
                      configs: 'deployment-configs/beckn-transport-deploy.yaml',
                      enableConfigSubstitution: true
                    )
                kubernetesDeploy(
                      kubeconfigId: 'eks-user-staging',
                      configs: 'deployment-configs/beckn-transport-allocation-service-deploy.yaml',
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
