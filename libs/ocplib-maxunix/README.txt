This library is only used when bootstrapping ocp-build.

Since ocp-pp is not available during bootstrapping, and ocp-pp is
necessary to build ocplib-unix to cope with incompatibilities in Unix
primitives introduced at 4.05.0, we just use this library instead.

