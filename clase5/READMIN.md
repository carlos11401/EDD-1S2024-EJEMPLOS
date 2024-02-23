# JSON-FORTRAN install

First you need to install [CMAKE](https://cmake.org/download/). For windows choose this **Windows x64 Installer**. When you finish to install it can test running this:

```cmake --version```

Now you need to clone the repository where is json-fortran.

`git clone https://github.com/jacobwilliams/json-fortran`

On json-fortran directory create '**build**' directory 

```
cd json-fortran 
mkdir build
```

On build directory open a **command prompt as administrator** and execute the next commands:

```
cmake -G "Unix Makefiles" ..
make
make install
```

If your directory structure is like this:
```
- example/ 
  - json-fortran/
  - main.f90
```

To create the executable we need to include the **include library** and **libjsonfortran.a** that are on **json-fortran/build/**, for that you need to open a terminal on example directory and run the next command:

`
gfortran -I./json-fortran/build/include -o main main.f90 ./json-fortran/build/lib/libjsonfortran.a
`

That command will create an executable file, so you just need to run it.

`./main`

## Visual Studio Code

If your using **Visual Studio Code** could be posible that show you an error to import the library.

`use json_module`

To resolve it you can create a **settings.json** on .vscode directory and add this:

```
{
  "fortran.linter.includePaths": ["C:/Users/USER/Documents/GIT/EDD-1S2024-EJEMPLOS/clase5/json-example/json-fortran/build/include"]
}
```

Just change the **path** where you have the include library.