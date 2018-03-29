IF NOT EXIST paket.lock (
    START /WAIT .paket/paket.exe install
)
dotnet restore src/Rosdex.Services.ProductManager
dotnet build src/Rosdex.Services.ProductManager

