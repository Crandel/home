package exmpl

import "runtime"

func GetOsType() string {
    return runtime.GOOS
}
