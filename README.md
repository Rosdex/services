# Rosdex.Services.ProductManager

A [Giraffe](https://github.com/giraffe-fsharp/Giraffe) web application, which has been created via the `dotnet new giraffe` command.

## Build and test the application

### Windows

Run the `build.bat` script in order to restore, build and test (if you've selected to include tests) the application:

```
> ./build.bat
```

### Linux/macOS

Run the `build.sh` script in order to restore, build and test (if you've selected to include tests) the application:

```
$ ./build.sh
```

## Run the application

After a successful build you can start the web application by executing the following command in your terminal:

* -cpje - Category Prediction Jobs Endpoint - необязательный аргумент (дефолт: http://localhost:58888/jobs).

```
dotnet run src/Rosdex.Services.ProductManager -cpje "http://localhost:5001/jobs"
```

After the application has started visit [http://localhost:54496](http://localhost:54496) in your preferred browser. Где ничего не увидите.