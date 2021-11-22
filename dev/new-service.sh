echo 'Type in name of a new service in kebab case:'
read name
cp -r ./app/example-service ./app/${name}
echo ${name} | sed -i "s/example-service/${name}/g" ./app/${name}/package.yaml
rm ./app/${name}/example-service.cabal
tree ./app/${name}
