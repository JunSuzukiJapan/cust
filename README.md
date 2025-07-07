# Programming Language Cust

Customized C like Rust

# Requirement

- LLVM
- Standard C Libraries & Headers
- C Toolchain

# Unsupported yet

- preprocessor
- standard libs & headers

# Usage

## Compile

```
cust -c <filename>.cust
```

## Run

```
lli <filename>.ll
```

# Feature

## impl

```
int printf(char* format, ...);

struct Circle {
    int x, y;
    int radius;
};

impl Circle {
    const int PI100 = 314;

    Self new(int x, int y, int radius) {
        return Circle {
            x: x;
            y: y;
            radius: radius;
        };
    }

    int area(&self) {
        return self.radius * Self::PI100 / 100;
    }
}

int main() {
    Circle c = Circle::new(0, 0, 100);
    printf("area: %d\n", c.area());

    return c.area();
}
```

## tuple

```
int printf(char* format, ...);

$<int, int> tpl = $(1, 2);

int main(){
    return tpl.0 + tpl.1;
}
```

## extended enum

```
enum SomeEnum {
    Foo (int, int),
    Bar {
        int x;
        int y;
        int z;
    },
    Zot,
};

SomeEnum some_enum = SomeEnum::Bar{ x: 1; y: 2; z: 3;};

int main() {
    if let (SomeEnum::Bar { x, y, z } = some_enum) {
        return x + y + z;
    }else{
        return 0;
    }
}
```

## if-let

```
int main(){
    if let (x @ ('a' | 'b') = 'c') {
        if(x == 'a'){
            return 10;
        }else if(x == 'b') {
            return 20;
        }
        return 30;
    }else{
        return 2;
    }

    return 3;
}
```

## do match

```
int test(int input){
    int output = 0;

    do match (input) {
        0 => {
            output = 1;
        },
        1 => {
            output = 2;
        },
        2 => {
            output = 3;
        },
        _ => {
            output = 4;
        }
    }

    return output;
}

int main() {
    int result = test(1);
    return result;
}
```

## generics

```
struct SomeType<T, U> {
    T x;
    U y;
};

int main(){
    struct SomeType<int, int> some = SomeType<int, int>{x:10; y:20;};
    return some.x + some.y;
}
```


# FAQ

- Q. Are there plans to implement 'lifetime in the future?
- A. Never

## LICENSE

[LICENSE](LICENSE)
