package web

import (
	"encoding/json"
	"fmt"
	"html/template"
	"js-analyzer/internal/analyzer"
	"log"
	"net/http"
	"strconv"
	"strings"
	"time"

	jsoniter "github.com/json-iterator/go"
	"github.com/gorilla/mux"
)

// Handler maneja las peticiones web
type Handler struct {
	analyzer  *analyzer.JSAnalyzer
	templates *template.Template
	json      jsoniter.API
	config    HandlerConfig
}

// HandlerConfig configuración del handler
type HandlerConfig struct {
	MaxCodeSize     int           `json:"maxCodeSize"`
	RequestTimeout  time.Duration `json:"requestTimeout"`
	EnableCORS      bool          `json:"enableCORS"`
	EnableCache     bool          `json:"enableCache"`
	VerboseLogging  bool          `json:"verboseLogging"`
	RateLimitRPM    int          `json:"rateLimitRPM"`
}

// DefaultHandlerConfig configuración por defecto
var DefaultHandlerConfig = HandlerConfig{
	MaxCodeSize:     1000000, // 1MB
	RequestTimeout:  30 * time.Second,
	EnableCORS:      true,
	EnableCache:     true,
	VerboseLogging:  false,
	RateLimitRPM:    100, // 100 requests per minute
}

// NewHandler crea un nuevo manejador web
func NewHandler() *Handler {
	return NewHandlerWithConfig(DefaultHandlerConfig)
}

// NewHandlerWithConfig crea un nuevo manejador con configuración personalizada
func NewHandlerWithConfig(config HandlerConfig) *Handler {
	// Configurar json-iterator para mejor rendimiento
	json := jsoniter.ConfigCompatibleWithStandardLibrary

	// Crear configuración optimizada del analizador
	analyzerConfig := analyzer.AnalyzerConfig{
		MaxTokens:        20000,
		MaxNesting:       100,
		TimeoutSeconds:   int(config.RequestTimeout.Seconds()),
		EnableCache:      config.EnableCache,
		EnableStatistics: true,
		VerboseLogging:   config.VerboseLogging,
	}

	return &Handler{
		analyzer: analyzer.NewJSAnalyzerWithConfig(analyzerConfig),
		json:     json,
		config:   config,
	}
}

// LoadTemplates carga las plantillas HTML
func (h *Handler) LoadTemplates() error {
	var err error
	h.templates, err = template.New("").Funcs(template.FuncMap{
		"formatTime": func(d time.Duration) string {
			if d < time.Microsecond {
				return fmt.Sprintf("%.2fns", float64(d.Nanoseconds()))
			} else if d < time.Millisecond {
				return fmt.Sprintf("%.2fμs", float64(d.Microseconds()))
			} else if d < time.Second {
				return fmt.Sprintf("%.2fms", float64(d.Nanoseconds())/1e6)
			}
			return d.String()
		},
		"formatJSON": func(v interface{}) string {
			b, err := h.json.MarshalIndent(v, "", "  ")
			if err != nil {
				return "{}"
			}
			return string(b)
		},
		"errorClass": func(severity string) string {
			switch severity {
			case "error":
				return "error"
			case "warning":
				return "warning"
			case "info":
				return "info"
			default:
				return "info"
			}
		},
		"inc": func(i int) int {
			return i + 1
		},
		"tokenCategoryClass": func(category string) string {
			switch strings.ToLower(category) {
			case "keyword":
				return "token-keyword"
			case "operator":
				return "token-operator"
			case "literal":
				return "token-literal"
			case "identifier":
				return "token-identifier"
			case "delimiter":
				return "token-delimiter"
			case "comment":
				return "token-comment"
			default:
				return "token-default"
			}
		},
		"symbolTypeClass": func(symbolType string) string {
			switch strings.ToLower(symbolType) {
			case "variable":
				return "symbol-variable"
			case "function":
				return "symbol-function"
			case "parameter":
				return "symbol-parameter"
			default:
				return "symbol-default"
			}
		},
		"formatComplexity": func(complexity int) string {
			switch {
			case complexity <= 5:
				return fmt.Sprintf("Low (%d)", complexity)
			case complexity <= 10:
				return fmt.Sprintf("Medium (%d)", complexity)
			case complexity <= 20:
				return fmt.Sprintf("High (%d)", complexity)
			default:
				return fmt.Sprintf("Very High (%d)", complexity)
			}
		},
		"complexityClass": func(complexity int) string {
			switch {
			case complexity <= 5:
				return "complexity-low"
			case complexity <= 10:
				return "complexity-medium"
			case complexity <= 20:
				return "complexity-high"
			default:
				return "complexity-very-high"
			}
		},
		"formatCoverage": func(coverage float64) string {
			return fmt.Sprintf("%.1f%%", coverage)
		},
		"coverageClass": func(coverage float64) string {
			switch {
			case coverage >= 90:
				return "coverage-excellent"
			case coverage >= 70:
				return "coverage-good"
			case coverage >= 50:
				return "coverage-fair"
			default:
				return "coverage-poor"
			}
		},
	}).ParseGlob("templates/*.html")
	
	return err
}

// IndexHandler maneja la página principal
func (h *Handler) IndexHandler(w http.ResponseWriter, r *http.Request) {
	// Middleware de timeout
	ctx, cancel := context.WithTimeout(r.Context(), h.config.RequestTimeout)
	defer cancel()
	r = r.WithContext(ctx)

	if r.Method == http.MethodGet {
		h.renderIndex(w, r, nil, "")
		return
	}

	if r.Method == http.MethodPost {
		h.handleAnalysis(w, r)
		return
	}

	http.Error(w, "Method not allowed", http.StatusMethodNotAllowed)
}

// handleAnalysis maneja el análisis del código
func (h *Handler) handleAnalysis(w http.ResponseWriter, r *http.Request) {
	// Parsear el formulario con límite de tamaño
	r.Body = http.MaxBytesReader(w, r.Body, int64(h.config.MaxCodeSize))
	err := r.ParseForm()
	if err != nil {
		h.renderError(w, r, "Error parsing form", err)
		return
	}

	// Obtener parámetros del formulario
	code := r.FormValue("code")
	analysisTypeStr := r.FormValue("analysis_type")
	showTokens := r.FormValue("show_tokens") == "on"
	showAST := r.FormValue("show_ast") == "on"
	showSymbols := r.FormValue("show_symbols") == "on"
	
	// Opciones adicionales
	skipComments := r.FormValue("skip_comments") == "on"
	strictMode := r.FormValue("strict_mode") == "on"
	enableOptimizations := r.FormValue("enable_optimizations") == "on"

	// Validar código
	if strings.TrimSpace(code) == "" {
		h.renderIndex(w, r, nil, "Please enter some JavaScript code to analyze")
		return
	}

	// Verificar tamaño del código
	if len(code) > h.config.MaxCodeSize {
		h.renderIndex(w, r, nil, fmt.Sprintf("Code size exceeds maximum limit (%d characters)", h.config.MaxCodeSize))
		return
	}

	// Parsear tipo de análisis
	analysisType, err := strconv.Atoi(analysisTypeStr)
	if err != nil {
		analysisType = int(analyzer.FULL_ANALYSIS)
	}

	// Crear solicitud de análisis
	request := analyzer.AnalysisRequest{
		Code:         code,
		AnalysisType: analyzer.AnalysisType(analysisType),
		ShowTokens:   showTokens,
		ShowAST:      showAST,
		ShowSymbols:  showSymbols,
		Options: analyzer.AnalysisOptions{
			SkipComments:        skipComments,
			StrictMode:          strictMode,
			MaxTokenLength:      1000,
			EnableOptimizations: enableOptimizations,
		},
	}

	// Log request si está habilitado
	if h.config.VerboseLogging {
		log.Printf("Processing analysis request: type=%s, tokens=%v, ast=%v, symbols=%v", 
			request.AnalysisType.String(), showTokens, showAST, showSymbols)
	}

	// Realizar análisis
	result := h.analyzer.Analyze(request)

	// Log resultado si está habilitado
	if h.config.VerboseLogging {
		log.Printf("Analysis completed: success=%v, time=%v, tokens=%d", 
			result.Success, result.ExecutionTime, result.TokenCount)
	}

	// Agregar información del request al resultado para el template
	result.Code = code // Mantener código para el template

	// Renderizar resultado
	h.renderIndex(w, r, result, "")
}

// APIAnalyzeHandler maneja las peticiones de API
func (h *Handler) APIAnalyzeHandler(w http.ResponseWriter, r *http.Request) {
	// Middleware de timeout
	ctx, cancel := context.WithTimeout(r.Context(), h.config.RequestTimeout)
	defer cancel()
	r = r.WithContext(ctx)

	if r.Method != http.MethodPost {
		h.respondJSON(w, http.StatusMethodNotAllowed, map[string]string{
			"error": "Method not allowed",
		})
		return
	}

	// Limitar tamaño del body
	r.Body = http.MaxBytesReader(w, r.Body, int64(h.config.MaxCodeSize))

	// Parsear JSON
	var request analyzer.AnalysisRequest
	decoder := h.json.NewDecoder(r.Body)
	decoder.DisallowUnknownFields()
	
	if err := decoder.Decode(&request); err != nil {
		h.respondJSON(w, http.StatusBadRequest, map[string]interface{}{
			"error":   "Invalid JSON",
			"details": err.Error(),
		})
		return
	}

	// Validar solicitud
	if err := h.validateAPIRequest(request); err != nil {
		h.respondJSON(w, http.StatusBadRequest, map[string]interface{}{
			"error":   "Validation failed",
			"details": err.Error(),
		})
		return
	}

	// Log API request si está habilitado
	if h.config.VerboseLogging {
		log.Printf("API request: type=%s, code_length=%d", 
			request.AnalysisType.String(), len(request.Code))
	}

	// Realizar análisis
	result := h.analyzer.Analyze(request)

	// Log API resultado si está habilitado
	if h.config.VerboseLogging {
		log.Printf("API response: success=%v, time=%v", 
			result.Success, result.ExecutionTime)
	}

	// Responder con JSON
	h.respondJSON(w, http.StatusOK, result)
}

// validateAPIRequest valida la solicitud de API
func (h *Handler) validateAPIRequest(request analyzer.AnalysisRequest) error {
	if strings.TrimSpace(request.Code) == "" {
		return fmt.Errorf("code field is required and cannot be empty")
	}

	if len(request.Code) > h.config.MaxCodeSize {
		return fmt.Errorf("code size exceeds maximum limit (%d characters)", h.config.MaxCodeSize)
	}

	// Validar tipo de análisis
	if request.AnalysisType < 0 || request.AnalysisType > analyzer.FULL_ANALYSIS {
		return fmt.Errorf("invalid analysis type")
	}

	return nil
}

// HealthHandler maneja verificaciones de salud
func (h *Handler) HealthHandler(w http.ResponseWriter, r *http.Request) {
	response := map[string]interface{}{
		"status":       "healthy",
		"timestamp":    time.Now().UTC(),
		"version":      "1.0.0",
		"service":      "js-analyzer",
		"analyzer":     h.analyzer.Version(),
		"config": map[string]interface{}{
			"maxCodeSize":    h.config.MaxCodeSize,
			"requestTimeout": h.config.RequestTimeout.String(),
			"enableCache":    h.config.EnableCache,
		},
		"uptime": time.Since(startTime).String(),
	}
	
	h.respondJSON(w, http.StatusOK, response)
}

// ConfigHandler maneja la configuración
func (h *Handler) ConfigHandler(w http.ResponseWriter, r *http.Request) {
	if r.Method == http.MethodGet {
		response := map[string]interface{}{
			"handler":  h.config,
			"analyzer": h.analyzer.GetConfig(),
		}
		h.respondJSON(w, http.StatusOK, response)
		return
	}

	// POST para actualizar configuración (solo en modo desarrollo)
	if r.Method == http.MethodPost {
		var newConfig HandlerConfig
		if err := h.json.NewDecoder(r.Body).Decode(&newConfig); err != nil {
			h.respondJSON(w, http.StatusBadRequest, map[string]string{
				"error": "Invalid configuration JSON",
			})
			return
		}

		// Actualizar configuración
		h.config = newConfig
		
		h.respondJSON(w, http.StatusOK, map[string]interface{}{
			"message": "Configuration updated successfully",
			"config":  h.config,
		})
		return
	}

	http.Error(w, "Method not allowed", http.StatusMethodNotAllowed)
}

// StaticHandler maneja archivos estáticos con cache
func (h *Handler) StaticHandler(w http.ResponseWriter, r *http.Request) {
	// Configurar headers de cache
	if h.config.EnableCache {
		w.Header().Set("Cache-Control", "public, max-age=3600")
		w.Header().Set("ETag", `"js-analyzer-static-v1"`)
		
		// Verificar If-None-Match
		if match := r.Header.Get("If-None-Match"); match != "" {
			if strings.Contains(match, `"js-analyzer-static-v1"`) {
				w.WriteHeader(http.StatusNotModified)
				return
			}
		}
	}
	
	// Servir archivos estáticos
	fs := http.FileServer(http.Dir("static/"))
	http.StripPrefix("/static/", fs).ServeHTTP(w, r)
}

// renderIndex renderiza la página principal
func (h *Handler) renderIndex(w http.ResponseWriter, r *http.Request, result *analyzer.AnalysisResponse, errorMsg string) {
	data := struct {
		Result   *analyzer.AnalysisResponse
		Error    string
		Title    string
		Examples []ExampleCode
		Config   map[string]interface{}
	}{
		Result:   result,
		Error:    errorMsg,
		Title:    "JavaScript Code Analyzer",
		Examples: getExampleCodes(),
		Config: map[string]interface{}{
			"maxCodeSize": h.config.MaxCodeSize,
			"enableCache": h.config.EnableCache,
		},
	}

	w.Header().Set("Content-Type", "text/html; charset=utf-8")
	
	if err := h.templates.ExecuteTemplate(w, "index.html", data); err != nil {
		log.Printf("Error rendering template: %v", err)
		http.Error(w, "Internal server error", http.StatusInternalServerError)
	}
}

// renderError renderiza una página de error
func (h *Handler) renderError(w http.ResponseWriter, r *http.Request, message string, err error) {
	log.Printf("Error: %s - %v", message, err)
	
	data := struct {
		Title   string
		Message string
		Error   string
	}{
		Title:   "Error - JavaScript Code Analyzer",
		Message: message,
		Error:   err.Error(),
	}

	w.Header().Set("Content-Type", "text/html; charset=utf-8")
	w.WriteHeader(http.StatusInternalServerError)
	
	if err := h.templates.ExecuteTemplate(w, "error.html", data); err != nil {
		log.Printf("Error rendering error template: %v", err)
		http.Error(w, "Internal server error", http.StatusInternalServerError)
	}
}

// respondJSON responde con JSON optimizado
func (h *Handler) respondJSON(w http.ResponseWriter, status int, data interface{}) {
	w.Header().Set("Content-Type", "application/json")
	w.WriteHeader(status)
	
	encoder := h.json.NewEncoder(w)
	if h.config.VerboseLogging {
		encoder.SetIndent("", "  ")
	}
	
	if err := encoder.Encode(data); err != nil {
		log.Printf("Error encoding JSON response: %v", err)
		http.Error(w, `{"error": "Internal server error"}`, http.StatusInternalServerError)
	}
}

// ExampleCode representa un ejemplo de código
type ExampleCode struct {
	Name        string `json:"name"`
	Description string `json:"description"`
	Code        string `json:"code"`
	Category    string `json:"category"`
	Difficulty  string `json:"difficulty"`
}

// getExampleCodes retorna ejemplos de código predefinidos mejorados
func getExampleCodes() []ExampleCode {
	return []ExampleCode{
		{
			Name:        "Variables y Funciones ES6+",
			Description: "Ejemplo con let, const, arrow functions y template literals",
			Category:    "basic",
			Difficulty:  "beginner",
			Code: `const PI = 3.14159;
let radius = 5;

// Arrow function con template literal
const calculateArea = (r) => \`Area: \${PI * r * r}\`;

// Destructuring y spread operator
const [first, ...rest] = [1, 2, 3, 4, 5];
const obj = { x: 10, y: 20, ...{ z: 30 } };

console.log(calculateArea(radius));`,
		},
		{
			Name:        "Estructuras de Control Avanzadas",
			Description: "Ejemplo con estructuras de control y operadores modernos",
			Category:    "control",
			Difficulty:  "intermediate",
			Code: `function processData(data) {
    // Nullish coalescing y optional chaining
    const items = data?.items ?? [];
    
    for (const item of items) {
        if (item?.active && item.value > 0) {
            // Compound assignment operators
            item.score ||= 0;
            item.score += item.value;
            
            // Ternary operator
            item.status = item.score > 100 ? 'high' : 'normal';
        }
    }
    
    return items.filter(item => item.status === 'high');
}

const result = processData({ 
    items: [
        { active: true, value: 150 },
        { active: false, value: 50 }
    ]
});`,
		},
		{
			Name:        "Errores Semánticos Comunes",
			Description: "Código con diversos errores para probar el analizador",
			Category:    "errors",
			Difficulty:  "intermediate",
			Code: `// Variable no declarada
resultado = x + y;

// Reasignación de const
const IMMUTABLE_VALUE = 42;
IMMUTABLE_VALUE = 100;

// Variable declarada pero no utilizada
let unusedVariable = "This is never used";

// Función llamada como variable
function calculateSum(a, b) {
    return a + b;
}
let invalidOperation = calculateSum + 5;

// Redeclaración con let
let userName = "Alice";
let userName = "Bob";

// Return fuera de función
return "Invalid return";

// Parámetros duplicados
function duplicate(param, param) {
    return param;
}`,
		},
		{
			Name:        "Ejemplo Complejo con Clases",
			Description: "Código complejo con clases, módulos y características ES6+",
			Category:    "complex",
			Difficulty:  "advanced",
			Code: `class DataProcessor {
    #privateField = new Map();
    
    constructor(config = {}) {
        this.config = { timeout: 5000, ...config };
        this.#privateField.set('initialized', Date.now());
    }
    
    async processAsync(data) {
        try {
            const processed = await this.#transformData(data);
            return { success: true, data: processed };
        } catch (error) {
            return { success: false, error: error.message };
        }
    }
    
    #transformData(data) {
        return new Promise((resolve, reject) => {
            setTimeout(() => {
                if (!data || data.length === 0) {
                    reject(new Error('No data provided'));
                    return;
                }
                
                const transformed = data.map((item, index) => ({
                    id: index,
                    value: item * 2,
                    timestamp: Date.now()
                }));
                
                resolve(transformed);
            }, this.config.timeout);
        });
    }
    
    static create(config) {
        return new DataProcessor(config);
    }
}

// Usage
const processor = DataProcessor.create({ timeout: 1000 });
const result = await processor.processAsync([1, 2, 3, 4, 5]);`,
		},
		{
			Name:        "Funciones Modernas y Operadores",
			Description: "Características modernas de JavaScript",
			Category:    "modern",
			Difficulty:  "intermediate",
			Code: `// Generadores y iteradores
function* fibonacciGenerator() {
    let [a, b] = [0, 1];
    while (true) {
        yield a;
        [a, b] = [b, a + b];
    }
}

// WeakMap y Symbol
const cache = new WeakMap();
const SECRET_KEY = Symbol('secret');

class APIClient {
    constructor(baseURL) {
        this.baseURL = baseURL;
        this[SECRET_KEY] = 'api-secret-key';
    }
    
    // Method with default parameters and rest syntax
    async request(endpoint, options = {}, ...middleware) {
        const { method = 'GET', headers = {} } = options;
        
        // Template literals and computed properties
        const url = \`\${this.baseURL}\${endpoint}\`;
        const config = {
            method,
            headers: {
                'Content-Type': 'application/json',
                ...headers
            }
        };
        
        // Apply middleware
        middleware.forEach(fn => fn(config));
        
        try {
            const response = await fetch(url, config);
            return await response.json();
        } catch (error) {
            throw new Error(\`Request failed: \${error.message}\`);
        }
    }
}

// Usage with destructuring and async/await
const client = new APIClient('https://api.example.com');
const { data } = await client.request('/users', { method: 'GET' });`,
		},
	}
}

// SetupRoutes configura las rutas del servidor
func (h *Handler) SetupRoutes() *mux.Router {
	r := mux.NewRouter()
	
	// Rutas principales
	r.HandleFunc("/", h.IndexHandler).Methods("GET", "POST")
	r.HandleFunc("/health", h.HealthHandler).Methods("GET")
	r.HandleFunc("/config", h.ConfigHandler).Methods("GET", "POST")
	
	// API endpoints
	api := r.PathPrefix("/api").Subrouter()
	api.HandleFunc("/analyze", h.APIAnalyzeHandler).Methods("POST")
	api.HandleFunc("/health", h.HealthHandler).Methods("GET")
	api.HandleFunc("/config", h.ConfigHandler).Methods("GET")
	
	// Archivos estáticos
	r.PathPrefix("/static/").HandlerFunc(h.StaticHandler)
	
	// Middleware
	r.Use(h.loggingMiddleware)
	if h.config.EnableCORS {
		r.Use(h.corsMiddleware)
	}
	r.Use(h.securityMiddleware)
	
	return r
}

// Middleware

// loggingMiddleware middleware para logging de peticiones
func (h *Handler) loggingMiddleware(next http.Handler) http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		start := time.Now()
		
		// Crear un ResponseWriter que capture el status code
		wrapped := &responseWriter{ResponseWriter: w, statusCode: http.StatusOK}
		
		next.ServeHTTP(wrapped, r)
		
		duration := time.Since(start)
		
		if h.config.VerboseLogging || wrapped.statusCode >= 400 {
			log.Printf("%s %s %d %v %s %s",
				r.Method,
				r.RequestURI,
				wrapped.statusCode,
				duration,
				r.RemoteAddr,
				r.UserAgent(),
			)
		}
	})
}

// corsMiddleware middleware para CORS
func (h *Handler) corsMiddleware(next http.Handler) http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Access-Control-Allow-Origin", "*")
		w.Header().Set("Access-Control-Allow-Methods", "GET, POST, PUT, DELETE, OPTIONS")
		w.Header().Set("Access-Control-Allow-Headers", "Content-Type, Authorization")
		w.Header().Set("Access-Control-Max-Age", "86400")
		
		if r.Method == "OPTIONS" {
			w.WriteHeader(http.StatusOK)
			return
		}
		
		next.ServeHTTP(w, r)
	})
}

// securityMiddleware middleware para headers de seguridad
func (h *Handler) securityMiddleware(next http.Handler) http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		// Headers de seguridad básicos
		w.Header().Set("X-Content-Type-Options", "nosniff")
		w.Header().Set("X-Frame-Options", "DENY")
		w.Header().Set("X-XSS-Protection", "1; mode=block")
		w.Header().Set("Referrer-Policy", "strict-origin-when-cross-origin")
		
		next.ServeHTTP(w, r)
	})
}

// responseWriter wrapper para capturar el status code
type responseWriter struct {
	http.ResponseWriter
	statusCode int
}

func (rw *responseWriter) WriteHeader(code int) {
	rw.statusCode = code
	rw.ResponseWriter.WriteHeader(code)
}

// Variables globales
var startTime = time.Now()

// Import context para timeout
import "context"