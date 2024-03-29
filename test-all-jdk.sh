#! /usr/bin/env bash
set -e

if ! awk '{ split($3, a, "-"); if (a[1] < "17.05") exit 1 }' < <(docker --version); then
    >&2 echo 'this script requires docker 17.05 or greater'
    exit 1
fi

ns=jgpc42
lib=$ns/jmh-clojure
deps=$lib.deps

jdks=(
    openjdk:8
    openjdk:9
    openjdk:10
    openjdk:11
    openjdk:12
    openjdk:13
    openjdk:14
    openjdk:15
    openjdk:16
    openjdk:17
    openjdk:18
)

mkdir -p target
dockerfile=$(mktemp -p target)

docker build -t "$deps" .

for jdk in "${jdks[@]}"; do
    tag=$lib.${jdk#$ns/}
    cat <<EOF > "$dockerfile"
    FROM $jdk
    COPY . /root
    COPY --from=$deps /root /root
    WORKDIR /root
    ENV LEIN_ROOT=1
    CMD ["./lein", "test-all"]
EOF
    docker build -t "$tag" -f "$dockerfile" .
    docker run --rm "$tag"
done

rm -f "$dockerfile"
