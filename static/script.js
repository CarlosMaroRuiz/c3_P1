/**
 * JavaScript Code Analyzer - Frontend Script
 * Funcionalidad interactiva para la interfaz de usuario
 */

class JSAnalyzerUI {
    constructor() {
        this.initializeElements();
        this.bindEvents();
        this.setupCharacterCounter();
        this.setupFormValidation();
        this.setupExamples();
        this.setupAccessibility();
        this.setupAnimations();
        this.setupAPI();
    }

    initializeElements() {
        // Elementos principales
        this.codeTextarea = document.getElementById('code');
        this.analyzeForm = document.getElementById('analyzeForm');
        this.clearBtn = document.getElementById('clearBtn');
        this.charCount = document.getElementById('charCount');
        
        // Opciones de análisis
        this.analysisTypeSelect = document.getElementById('analysis_type');
        this.showTokensCheckbox = document.querySelector('input[name="show_tokens"]');
        this.showASTCheckbox = document.querySelector('input[name="show_ast"]');
        this.showSymbolsCheckbox = document.querySelector('input[name="show_symbols"]');
        
        // Opciones adicionales
        this.skipCommentsCheckbox = document.querySelector('input[name="skip_comments"]');
        this.strictModeCheckbox = document.querySelector('input[name="strict_mode"]');
        this.optimizationsCheckbox = document.querySelector('input[name="enable_optimizations"]');
        
        // Ejemplos
        this.exampleCards = document.querySelectorAll('.example-card');
        
        // Configuración
        this.maxCodeSize = parseInt(this.codeTextarea.getAttribute('maxlength')) || 1000000;
        
        console.log('🔍 JS Analyzer UI initialized');
    }

    bindEvents() {
        // Formulario
        this.analyzeForm?.addEventListener('submit', this.handleFormSubmit.bind(this));
        this.clearBtn?.addEventListener('click', this.handleClearCode.bind(this));
        
        // Textarea
        this.codeTextarea?.addEventListener('input', this.handleCodeInput.bind(this));
        this.codeTextarea?.addEventListener('paste', this.handleCodePaste.bind(this));
        this.codeTextarea?.addEventListener('keydown', this.handleCodeKeydown.bind(this));
        
        // Ejemplos
        this.exampleCards.forEach(card => {
            card.addEventListener('click', this.handleExampleClick.bind(this));
            card.addEventListener('keydown', this.handleExampleKeydown.bind(this));
        });
        
        // Opciones de análisis
        this.analysisTypeSelect?.addEventListener('change', this.handleAnalysisTypeChange.bind(this));
        
        // Eventos de ventana
        window.addEventListener('beforeunload', this.handleBeforeUnload.bind(this));
        window.addEventListener('resize', this.handleWindowResize.bind(this));
        
        // Atajos de teclado
        document.addEventListener('keydown', this.handleGlobalKeydown.bind(this));
    }

    setupCharacterCounter() {
        if (!this.codeTextarea || !this.charCount) return;
        
        this.updateCharacterCount();
    }

    updateCharacterCount() {
        const currentLength = this.codeTextarea.value.length;
        const percentage = (currentLength / this.maxCodeSize) * 100;
        
        this.charCount.textContent = currentLength.toLocaleString();
        
        // Actualizar clases de advertencia
        this.charCount.classList.remove('warning', 'error');
        if (percentage > 90) {
            this.charCount.classList.add('error');
        } else if (percentage > 75) {
            this.charCount.classList.add('warning');
        }
        
        // Actualizar progreso visual si existe
        const progressBar = document.querySelector('.char-progress');
        if (progressBar) {
            progressBar.style.width = `${Math.min(percentage, 100)}%`;
        }
    }

    setupFormValidation() {
        if (!this.analyzeForm) return;
        
        // Validación en tiempo real
        this.codeTextarea?.addEventListener('blur', this.validateCode.bind(this));
        
        // Validación antes del envío
        this.analyzeForm.addEventListener('submit', (e) => {
            if (!this.validateForm()) {
                e.preventDefault();
                return false;
            }
        });
    }

    validateCode() {
        const code = this.codeTextarea.value.trim();
        const isValid = code.length > 0 && code.length <= this.maxCodeSize;
        
        this.codeTextarea.classList.toggle('invalid', !isValid);
        
        if (!isValid && code.length === 0) {
            this.showValidationMessage('Por favor, ingresa código JavaScript para analizar');
        } else if (!isValid && code.length > this.maxCodeSize) {
            this.showValidationMessage(`El código excede el límite de ${this.maxCodeSize.toLocaleString()} caracteres`);
        } else {
            this.hideValidationMessage();
        }
        
        return isValid;
    }

    validateForm() {
        const isCodeValid = this.validateCode();
        
        if (!isCodeValid) {
            this.codeTextarea?.focus();
            return false;
        }
        
        return true;
    }

    showValidationMessage(message) {
        this.hideValidationMessage();
        
        const validationDiv = document.createElement('div');
        validationDiv.className = 'validation-message alert alert-error';
        validationDiv.textContent = message;
        
        this.codeTextarea.parentNode.insertBefore(validationDiv, this.codeTextarea.nextSibling);
        
        // Auto-remover después de 5 segundos
        setTimeout(() => this.hideValidationMessage(), 5000);
    }

    hideValidationMessage() {
        const existing = document.querySelector('.validation-message');
        if (existing) {
            existing.remove();
        }
    }

    setupExamples() {
        this.exampleCards.forEach((card, index) => {
            // Hacer accesible por teclado
            card.setAttribute('tabindex', '0');
            card.setAttribute('role', 'button');
            card.setAttribute('aria-label', `Cargar ejemplo: ${card.querySelector('h4')?.textContent}`);
            
            // Animación de entrada
            card.style.animationDelay = `${index * 0.1}s`;
            card.classList.add('fade-in');
        });
    }

    handleExampleClick(event) {
        const card = event.currentTarget;
        const codeData = card.getAttribute('data-code');
        
        if (codeData && this.codeTextarea) {
            // Confirmar si hay código existente
            if (this.codeTextarea.value.trim() && !this.confirmCodeReplace()) {
                return;
            }
            
            this.loadExampleCode(codeData, card);
        }
    }

    handleExampleKeydown(event) {
        if (event.key === 'Enter' || event.key === ' ') {
            event.preventDefault();
            this.handleExampleClick(event);
        }
    }

    confirmCodeReplace() {
        return confirm('¿Estás seguro de que quieres reemplazar el código actual con este ejemplo?');
    }

    loadExampleCode(code, card) {
        if (!this.codeTextarea) return;
        
        // Efecto visual
        card.classList.add('selected');
        setTimeout(() => card.classList.remove('selected'), 300);
        
        // Cargar código con animación
        this.codeTextarea.style.opacity = '0.5';
        
        setTimeout(() => {
            this.codeTextarea.value = code;
            this.updateCharacterCount();
            this.codeTextarea.style.opacity = '1';
            this.codeTextarea.focus();
            
            // Notificación
            this.showNotification('✅ Ejemplo cargado exitosamente', 'success');
            
            // Auto-scroll al textarea
            this.codeTextarea.scrollIntoView({ 
                behavior: 'smooth', 
                block: 'center' 
            });
        }, 150);
    }

    handleFormSubmit(event) {
        // Agregar estado de carga
        const submitBtn = this.analyzeForm.querySelector('button[type="submit"]');
        if (submitBtn) {
            this.setLoadingState(submitBtn, true);
        }
        
        // Validar antes del envío
        if (!this.validateForm()) {
            event.preventDefault();
            this.setLoadingState(submitBtn, false);
            return false;
        }
        
        // Guardar código en localStorage para recuperación
        this.saveCodeToStorage();
        
        // Mostrar notificación
        this.showNotification('🔍 Analizando código...', 'info');
    }

    handleClearCode() {
        if (this.codeTextarea.value.trim() && !confirm('¿Estás seguro de que quieres limpiar todo el código?')) {
            return;
        }
        
        this.clearCode();
    }

    clearCode() {
        if (this.codeTextarea) {
            this.codeTextarea.value = '';
            this.updateCharacterCount();
            this.hideValidationMessage();
            this.codeTextarea.focus();
            
            this.showNotification('🗑️ Código limpiado', 'info');
        }
        
        // Limpiar checkboxes si se desea
        const resetOptions = document.querySelector('.reset-options-on-clear');
        if (resetOptions) {
            this.resetAnalysisOptions();
        }
    }

    resetAnalysisOptions() {
        // Resetear a valores por defecto
        if (this.analysisTypeSelect) this.analysisTypeSelect.selectedIndex = 3;
        
        const checkboxes = [
            this.showTokensCheckbox,
            this.showASTCheckbox, 
            this.showSymbolsCheckbox,
            this.skipCommentsCheckbox,
            this.strictModeCheckbox,
            this.optimizationsCheckbox
        ];
        
        checkboxes.forEach(checkbox => {
            if (checkbox) checkbox.checked = false;
        });
    }

    handleCodeInput(event) {
        this.updateCharacterCount();
        this.hideValidationMessage();
        
        // Guardar automáticamente cada 5 segundos
        clearTimeout(this.autoSaveTimeout);
        this.autoSaveTimeout = setTimeout(() => {
            this.saveCodeToStorage();
        }, 5000);
    }

    handleCodePaste(event) {
        setTimeout(() => {
            this.updateCharacterCount();
            this.validateCode();
        }, 10);
    }

    handleCodeKeydown(event) {
        // Atajos de teclado útiles
        if (event.ctrlKey || event.metaKey) {
            switch (event.key) {
                case 'Enter':
                    event.preventDefault();
                    this.analyzeForm?.dispatchEvent(new Event('submit'));
                    break;
                case 'l':
                    event.preventDefault();
                    this.clearCode();
                    break;
            }
        }
        
        // Tab para indentación
        if (event.key === 'Tab') {
            event.preventDefault();
            this.insertTab();
        }
    }

    insertTab() {
        const start = this.codeTextarea.selectionStart;
        const end = this.codeTextarea.selectionEnd;
        const value = this.codeTextarea.value;
        
        this.codeTextarea.value = value.substring(0, start) + '\t' + value.substring(end);
        this.codeTextarea.selectionStart = this.codeTextarea.selectionEnd = start + 1;
        
        this.updateCharacterCount();
    }

    handleAnalysisTypeChange() {
        const analysisType = parseInt(this.analysisTypeSelect.value);
        
        // Sugerir opciones basadas en el tipo de análisis
        switch (analysisType) {
            case 0: // Solo Léxico
                this.showTokensCheckbox.checked = true;
                this.showASTCheckbox.checked = false;
                this.showSymbolsCheckbox.checked = false;
                break;
            case 1: // Solo Sintáctico
                this.showTokensCheckbox.checked = false;
                this.showASTCheckbox.checked = true;
                this.showSymbolsCheckbox.checked = false;
                break;
            case 2: // Solo Semántico
                this.showTokensCheckbox.checked = false;
                this.showASTCheckbox.checked = false;
                this.showSymbolsCheckbox.checked = true;
                break;
            case 3: // Análisis Completo
                this.showTokensCheckbox.checked = true;
                this.showASTCheckbox.checked = true;
                this.showSymbolsCheckbox.checked = true;
                break;
        }
    }

    handleBeforeUnload(event) {
        const hasUnsavedCode = this.codeTextarea?.value.trim() && 
                               this.codeTextarea.value !== this.getStoredCode();
        
        if (hasUnsavedCode) {
            event.preventDefault();
            event.returnValue = '';
            return '';
        }
    }

    handleWindowResize() {
        // Ajustar altura del textarea en dispositivos móviles
        if (window.innerWidth < 768 && this.codeTextarea) {
            const viewportHeight = window.innerHeight;
            const maxHeight = Math.max(200, viewportHeight * 0.3);
            this.codeTextarea.style.maxHeight = `${maxHeight}px`;
        }
    }

    handleGlobalKeydown(event) {
        // Atajos globales
        if (event.ctrlKey || event.metaKey) {
            switch (event.key) {
                case '/':
                    event.preventDefault();
                    this.codeTextarea?.focus();
                    break;
                case 'k':
                    if (event.shiftKey) {
                        event.preventDefault();
                        this.showKeyboardShortcuts();
                    }
                    break;
            }
        }
        
        // Escape para cerrar modales
        if (event.key === 'Escape') {
            this.closeModals();
        }
    }

    setupAccessibility() {
        // Mejorar accesibilidad del formulario
        this.addAriaLabels();
        this.setupFocusManagement();
        this.setupScreenReaderAnnouncements();
    }

    addAriaLabels() {
        // Agregar etiquetas ARIA donde falten
        const elements = [
            { element: this.codeTextarea, label: 'Código JavaScript para analizar' },
            { element: this.analysisTypeSelect, label: 'Tipo de análisis a realizar' },
            { element: this.clearBtn, label: 'Limpiar código del editor' }
        ];
        
        elements.forEach(({ element, label }) => {
            if (element && !element.getAttribute('aria-label')) {
                element.setAttribute('aria-label', label);
            }
        });
    }

    setupFocusManagement() {
        // Mejorar navegación por teclado
        const focusableElements = this.analyzeForm?.querySelectorAll(
            'input, select, textarea, button, [tabindex]:not([tabindex="-1"])'
        );
        
        if (focusableElements) {
            this.focusableElements = Array.from(focusableElements);
        }
    }

    setupScreenReaderAnnouncements() {
        // Crear área para anuncios de screen reader
        if (!document.getElementById('sr-announcements')) {
            const srDiv = document.createElement('div');
            srDiv.id = 'sr-announcements';
            srDiv.setAttribute('aria-live', 'polite');
            srDiv.setAttribute('aria-atomic', 'true');
            srDiv.style.cssText = 'position: absolute; left: -10000px; width: 1px; height: 1px; overflow: hidden;';
            document.body.appendChild(srDiv);
        }
    }

    announceToScreenReader(message) {
        const srDiv = document.getElementById('sr-announcements');
        if (srDiv) {
            srDiv.textContent = message;
            setTimeout(() => srDiv.textContent = '', 1000);
        }
    }

    setupAnimations() {
        // Configurar animaciones de entrada
        this.observeElements();
        this.setupScrollAnimations();
    }

    observeElements() {
        const observer = new IntersectionObserver((entries) => {
            entries.forEach(entry => {
                if (entry.isIntersecting) {
                    entry.target.classList.add('visible');
                }
            });
        }, { threshold: 0.1 });
        
        // Observar elementos que deben animarse
        document.querySelectorAll('.fade-in, .slide-in').forEach(el => {
            observer.observe(el);
        });
    }

    setupScrollAnimations() {
        // Smooth scroll para enlaces internos
        document.querySelectorAll('a[href^="#"]').forEach(anchor => {
            anchor.addEventListener('click', (e) => {
                e.preventDefault();
                const target = document.querySelector(anchor.getAttribute('href'));
                if (target) {
                    target.scrollIntoView({ behavior: 'smooth', block: 'start' });
                }
            });
        });
    }

    setupAPI() {
        // Configurar cliente API para análisis en tiempo real (opcional)
        this.apiClient = new APIClient();
        this.setupRealTimeAnalysis();
    }

    setupRealTimeAnalysis() {
        // Análisis en tiempo real con debounce (opcional)
        let analysisTimeout;
        
        this.codeTextarea?.addEventListener('input', () => {
            clearTimeout(analysisTimeout);
            analysisTimeout = setTimeout(() => {
                this.performQuickAnalysis();
            }, 2000);
        });
    }

    async performQuickAnalysis() {
        const code = this.codeTextarea?.value?.trim();
        if (!code || code.length < 10) return;
        
        try {
            const result = await this.apiClient.quickAnalyze(code);
            this.showQuickResults(result);
        } catch (error) {
            console.warn('Quick analysis failed:', error);
        }
    }

    showQuickResults(result) {
        // Mostrar resultados rápidos (errores básicos)
        const quickResultsDiv = document.getElementById('quick-results');
        if (quickResultsDiv && result.errors?.length > 0) {
            quickResultsDiv.innerHTML = `
                <div class="quick-errors">
                    <span class="error-count">${result.errors.length} error(es) encontrado(s)</span>
                </div>
            `;
            quickResultsDiv.style.display = 'block';
        } else if (quickResultsDiv) {
            quickResultsDiv.style.display = 'none';
        }
    }

    // Utilidades
    setLoadingState(button, loading) {
        if (!button) return;
        
        if (loading) {
            button.disabled = true;
            button.classList.add('loading');
            button.setAttribute('data-original-text', button.textContent);
            button.innerHTML = '<span class="btn-icon">⏳</span> Analizando...';
        } else {
            button.disabled = false;
            button.classList.remove('loading');
            const originalText = button.getAttribute('data-original-text');
            if (originalText) {
                button.innerHTML = `<span class="btn-icon">🔍</span> ${originalText}`;
            }
        }
    }

    showNotification(message, type = 'info', duration = 3000) {
        // Crear y mostrar notificación toast
        const notification = document.createElement('div');
        notification.className = `notification notification-${type}`;
        notification.innerHTML = `
            <span class="notification-message">${message}</span>
            <button class="notification-close" aria-label="Cerrar notificación">&times;</button>
        `;
        
        // Estilos inline para la notificación
        notification.style.cssText = `
            position: fixed;
            top: 20px;
            right: 20px;
            z-index: 9999;
            padding: 15px 20px;
            border-radius: 8px;
            box-shadow: 0 4px 20px rgba(0, 0, 0, 0.15);
            transform: translateX(100%);
            transition: transform 0.3s ease;
            max-width: 400px;
            word-wrap: break-word;
        `;
        
        // Colores según el tipo
        const colors = {
            success: { bg: '#d4edda', border: '#c3e6cb', text: '#155724' },
            error: { bg: '#f8d7da', border: '#f5c6cb', text: '#721c24' },
            warning: { bg: '#fff3cd', border: '#ffeaa7', text: '#856404' },
            info: { bg: '#d1ecf1', border: '#bee5eb', text: '#0c5460' }
        };
        
        const color = colors[type] || colors.info;
        notification.style.background = color.bg;
        notification.style.border = `1px solid ${color.border}`;
        notification.style.color = color.text;
        
        document.body.appendChild(notification);
        
        // Animar entrada
        setTimeout(() => {
            notification.style.transform = 'translateX(0)';
        }, 10);
        
        // Manejar cierre
        const closeBtn = notification.querySelector('.notification-close');
        const closeNotification = () => {
            notification.style.transform = 'translateX(100%)';
            setTimeout(() => notification.remove(), 300);
        };
        
        closeBtn.addEventListener('click', closeNotification);
        
        // Auto-cierre
        if (duration > 0) {
            setTimeout(closeNotification, duration);
        }
        
        // Anunciar a screen readers
        this.announceToScreenReader(message);
    }

    saveCodeToStorage() {
        if (this.codeTextarea && window.localStorage) {
            try {
                localStorage.setItem('js-analyzer-code', this.codeTextarea.value);
                localStorage.setItem('js-analyzer-timestamp', Date.now().toString());
            } catch (e) {
                console.warn('Could not save to localStorage:', e);
            }
        }
    }

    getStoredCode() {
        if (window.localStorage) {
            try {
                return localStorage.getItem('js-analyzer-code') || '';
            } catch (e) {
                console.warn('Could not read from localStorage:', e);
            }
        }
        return '';
    }

    restoreCodeFromStorage() {
        const storedCode = this.getStoredCode();
        const timestamp = localStorage.getItem('js-analyzer-timestamp');
        
        // Solo restaurar si es del mismo día
        if (storedCode && timestamp) {
            const ageHours = (Date.now() - parseInt(timestamp)) / (1000 * 60 * 60);
            if (ageHours < 24 && this.codeTextarea && !this.codeTextarea.value.trim()) {
                this.codeTextarea.value = storedCode;
                this.updateCharacterCount();
                this.showNotification('📂 Código restaurado de la sesión anterior', 'info');
            }
        }
    }

    showKeyboardShortcuts() {
        const shortcuts = [
            { keys: 'Ctrl/Cmd + Enter', action: 'Analizar código' },
            { keys: 'Ctrl/Cmd + L', action: 'Limpiar código' },
            { keys: 'Ctrl/Cmd + /', action: 'Enfocar editor' },
            { keys: 'Tab', action: 'Insertar tabulación' },
            { keys: 'Ctrl/Cmd + Shift + K', action: 'Mostrar atajos' },
            { keys: 'Escape', action: 'Cerrar modales' }
        ];
        
        const modal = document.createElement('div');
        modal.className = 'shortcuts-modal';
        modal.innerHTML = `
            <div class="shortcuts-content">
                <h3>⌨️ Atajos de Teclado</h3>
                <div class="shortcuts-list">
                    ${shortcuts.map(s => `
                        <div class="shortcut-item">
                            <kbd>${s.keys}</kbd>
                            <span>${s.action}</span>
                        </div>
                    `).join('')}
                </div>
                <button class="btn btn-secondary" onclick="this.parentElement.parentElement.remove()">
                    Cerrar
                </button>
            </div>
        `;
        
        // Estilos para el modal
        modal.style.cssText = `
            position: fixed;
            top: 0;
            left: 0;
            width: 100%;
            height: 100%;
            background: rgba(0, 0, 0, 0.5);
            display: flex;
            align-items: center;
            justify-content: center;
            z-index: 10000;
        `;
        
        document.body.appendChild(modal);
        
        // Cerrar con click fuera
        modal.addEventListener('click', (e) => {
            if (e.target === modal) {
                modal.remove();
            }
        });
    }

    closeModals() {
        document.querySelectorAll('.shortcuts-modal, .notification').forEach(el => {
            el.remove();
        });
    }
}

// Cliente API para comunicación con el backend
class APIClient {
    constructor() {
        this.baseURL = '/api';
        this.timeout = 30000;
    }

    async quickAnalyze(code) {
        const payload = {
            code: code,
            analysisType: 0, // Solo léxico para análisis rápido
            showTokens: false,
            showAST: false,
            showSymbols: false,
            options: {
                skipComments: true,
                strictMode: false,
                maxTokenLength: 100
            }
        };

        return this.post('/analyze', payload);
    }

    async fullAnalyze(formData) {
        return this.post('/analyze', formData);
    }

    async post(endpoint, data) {
        try {
            const controller = new AbortController();
            const timeoutId = setTimeout(() => controller.abort(), this.timeout);

            const response = await fetch(`${this.baseURL}${endpoint}`, {
                method: 'POST',
                headers: {
                    'Content-Type': 'application/json',
                },
                body: JSON.stringify(data),
                signal: controller.signal
            });

            clearTimeout(timeoutId);

            if (!response.ok) {
                throw new Error(`HTTP ${response.status}: ${response.statusText}`);
            }

            return await response.json();
        } catch (error) {
            if (error.name === 'AbortError') {
                throw new Error('Request timeout');
            }
            throw error;
        }
    }

    async checkHealth() {
        try {
            const response = await fetch(`${this.baseURL}/health`);
            return response.ok;
        } catch {
            return false;
        }
    }
}

// Inicialización cuando el DOM esté listo
document.addEventListener('DOMContentLoaded', () => {
    // Inicializar la aplicación
    window.jsAnalyzerUI = new JSAnalyzerUI();
    
    // Restaurar código de sesión anterior
    window.jsAnalyzerUI.restoreCodeFromStorage();
    
    // Verificar estado del servicio
    window.jsAnalyzerUI.apiClient.checkHealth().then(isHealthy => {
        if (!isHealthy) {
            window.jsAnalyzerUI.showNotification(
                '⚠️ El servicio podría estar experimentando problemas',
                'warning',
                5000
            );
        }
    });
    
    console.log('🚀 JS Analyzer UI ready');
});

// Service Worker para modo offline (opcional)
if ('serviceWorker' in navigator) {
    window.addEventListener('load', () => {
        navigator.serviceWorker.register('/static/sw.js')
            .then(registration => {
                console.log('SW registered: ', registration);
            })
            .catch(registrationError => {
                console.log('SW registration failed: ', registrationError);
            });
    });
}

// Exportar para uso en otros scripts
window.JSAnalyzerUI = JSAnalyzerUI;
window.APIClient = APIClient;