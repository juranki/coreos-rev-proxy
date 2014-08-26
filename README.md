coreos-rev-proxy
================

Functionality:

 - listen to connections on port 80
 - route connections to backends according to configuration stored in etcd
 - routing is currently done using only the prefix of the requested path

configuration
-------------

Requires environvariables ETCD_URL and ETCD_PATH to access configuration in etcd. For example:

```
ETCD_URL=http://172.17.42.1:4001/v2/keys
ETCD_PATH=/http-service
```

coreos-rev-proxy watches changes in the specified etcd path and applies them immediatly.

etcd contents is of the form

```
core@core1 ~ $ etcdctl ls --recursive /http-service
/http-service/prefix1
/http-service/prefix2
core@core1 ~ $ etcdctl get /http-service/prefix1
10.11.100.100:8080
core@core1 ~ $
```

CoreOS and docker
-----------------

docker image is available at https://registry.hub.docker.com/u/juranki/coreos-rev-proxy/

On coreos you can use this unit to run it:

```
[Unit]
Description=CoreOSRevProxy
After=docker.service
After=etcd.service
Requires=docker.service
Requires=etcd.service

[Service]
TimeoutStartSec=30s
ExecStart=/usr/bin/docker run --rm --name coreos-rev-proxy -e "ETCD_URL=http://172.17.42.1:4001/v2/keys" -e "ETCD_PATH=/http-service" -p 80:80 juranki/coreos-rev-proxy
ExecStop=/usr/bin/docker stop coreos-rev-proxy

[Install]
WantedBy=multi-user.target
```
