package main

import (
	"context"
	"flag"
	"fmt"
	"js-analyzer/internal/web"
	"log"
	"net/http"
	"os"
	"os/signal"
	"syscall"
	"time"
)

// Version información de la aplicación
const (
	AppName    = "JS Code Analyzer"
	AppVersion = "1.0.0"
)

// Config configuración de la aplicación
type Config struct {
	Port            string        `json:"port"`
	Host            string        `json:"host"`
	ReadTimeout     time.Duration `json:"readTimeout"`
	WriteTimeout    time.Duration `json:"writeTimeout"`
	IdleTimeout     time.Duration `json:"idleTimeout"`
	ShutdownTimeout time.Duration `json:"shutdownTimeout"`
	Development     bool          `json:"development"`
}

// DefaultConfig configuración por defecto
var DefaultConfig = Config{
	Port:            "8080",
	Host:            "localhost",
	ReadTimeout:     15 * time.Second,
	WriteTimeout:    15 * time.Second,
	IdleTimeout:     60 * time.Second,
	ShutdownTimeout: 10 * time.Second,
	Development:     false,
}

func main() {
	// Configurar flags de línea de comandos
	var (
		port        = flag.String("port", DefaultConfig.Port, "Puerto del servidor")
		host        = flag.String("host", DefaultConfig.Host, "Host del servidor")
		development = flag.Bool("dev", DefaultConfig.Development, "Modo desarrollo")
		version     = flag.Bool("version", false, "Mostrar versión")
		help        = flag.Bool("help", false, "Mostrar ayuda")
	)
	flag.Parse()

	// Mostrar versión y salir
	if *version {
		fmt.Printf("%s v%s\n", AppName, AppVersion)
		os.Exit(0)
	}

	// Mostrar ayuda y salir
	if *help {
		printHelp()
		os.Exit(0)
	}

	// Crear configuración
	config := DefaultConfig
	config.Port = *port
	config.Host = *host
	config.Development = *development

	// Configurar logging
	if config.Development {
		log.SetFlags(log.LstdFlags | log.Lshortfile)
		log.Printf("Starting %s v%s in development mode", AppName, AppVersion)
	} else {
		log.SetFlags(log.LstdFlags)
		log.Printf("Starting %s v%s", AppName, AppVersion)
	}

	// Crear configuración del handler basada en el entorno
	handlerConfig := web.DefaultHandlerConfig
	if config.Development {
		handlerConfig.VerboseLogging = true
		handlerConfig.EnableCORS = true
		handlerConfig.MaxCodeSize = 2000000 // 2MB en desarrollo
	}

	// Crear handler
	handler := web.NewHandlerWithConfig(handlerConfig)

	// Cargar templates
	if err := handler.LoadTemplates(); err != nil {
		log.Fatalf("Error loading templates: %v", err)
	}

	// Configurar rutas
	router := handler.SetupRoutes()

	// Crear servidor HTTP
	server := &http.Server{
		Addr:         fmt.Sprintf("%s:%s", config.Host, config.Port),
		Handler:      router,
		ReadTimeout:  config.ReadTimeout,
		WriteTimeout: config.WriteTimeout,
		IdleTimeout:  config.IdleTimeout,
	}

	// Canal para escuchar errores del servidor
	serverErrors := make(chan error, 1)

	// Iniciar servidor en goroutine
	go func() {
		log.Printf("Server starting on http://%s:%s", config.Host, config.Port)
		if config.Development {
			log.Printf("Development mode enabled")
			log.Printf("API endpoint: http://%s:%s/api/analyze", config.Host, config.Port)
			log.Printf("Health check: http://%s:%s/health", config.Host, config.Port)
		}
		serverErrors <- server.ListenAndServe()
	}()

	// Canal para escuchar señales del sistema
	shutdown := make(chan os.Signal, 1)
	signal.Notify(shutdown, os.Interrupt, syscall.SIGTERM)

	// Esperar por shutdown o error del servidor
	select {
	case err := <-serverErrors:
		log.Fatalf("Server error: %v", err)

	case sig := <-shutdown:
		log.Printf("Shutdown signal received: %s", sig)

		// Crear contexto con timeout para shutdown
		ctx, cancel := context.WithTimeout(context.Background(), config.ShutdownTimeout)
		defer cancel()

		// Intentar graceful shutdown
		if err := server.Shutdown(ctx); err != nil {
			log.Printf("Graceful shutdown failed: %v", err)
			
			// Forzar cierre
			if err := server.Close(); err != nil {
				log.Printf("Force close failed: %v", err)
			}
		}

		log.Printf("Server stopped")
	}
}

// printHelp muestra la ayuda de la aplicación
func printHelp() {
	fmt.Printf(`%s v%s

Analizador de código JavaScript con análisis léxico, sintáctico y semántico.

Uso:
  js-analyzer [opciones]

Opciones:
  -port string     Puerto del servidor (default: %s)
  -host string     Host del servidor (default: %s)
  -dev             Ejecutar en modo desarrollo
  -version         Mostrar versión
  -help            Mostrar esta ayuda

Ejemplos:
  js-analyzer                    # Ejecutar con configuración por defecto
  js-analyzer -port 3000         # Ejecutar en puerto 3000
  js-analyzer -dev               # Ejecutar en modo desarrollo
  js-analyzer -host 0.0.0.0      # Permitir conexiones externas

API Endpoints:
  GET  /                    - Interfaz web principal
  POST /                    - Análisis vía formulario web
  POST /api/analyze         - Análisis vía API JSON
  GET  /api/health          - Estado de salud del servicio
  GET  /health              - Estado de salud (alias)
  GET  /static/*            - Archivos estáticos (CSS, JS)

Para más información, visita: https://github.com/tu-usuario/js-analyzer
`,
		AppName, AppVersion, DefaultConfig.Port, DefaultConfig.Host)
}