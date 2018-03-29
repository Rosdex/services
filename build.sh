if [ ! -e "paket.lock" ]
then
    exec mono .paket/paket.exe install
fi
dotnet restore src/Rosdex.Services.ProductManager
dotnet build src/Rosdex.Services.ProductManager

