//! # Random core generation engine for DOM

#![allow(dead_code, unused_variables)] /// Since the structures and variable are not constructed in `codegen.rs` `allow(dead_code) get rid warnings`

use crate::grammars::html::{HtmlElement, HtmlNode, HtmlTag, HtmlAttribute, HtmlTagWeight};
use crate::grammars::css::{CssProperty, CssTagWeights};
use crate::grammars::javascript::*;
use crate::utils::cli::Browser;
use std::fmt;
use rand::Rng;
use rand::distr::weighted::WeightedIndex;
use rand::distr::Distribution;
use rand_distr::Normal;
use std::fmt::Write;
use rand::prelude::IndexedRandom;
use std::collections::HashMap;

/// Include all browser type
impl fmt::Display for Browser {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", match self {
            Browser::Mozilla => "Mozilla",
            Browser::Safari => "Safari",
            Browser::Chrome => "Chrome",
            Browser::Others => "Standard",
        })
    }
}

/// Represents the full HTML AST
#[derive(Debug, Clone)]
pub struct HtmlAST {
    pub root: HtmlElement,
    pub elements: Vec<HtmlElement>,
}

/// Represents the Css AST
#[derive(Debug, Clone)]
pub struct CssAST {
    pub rules: Vec<CssRule>,
    pub browser_target: Option<Browser>,
}

/// Represents the Css rules
#[derive(Debug, Clone)]
pub struct CssRule {
    pub selector: CssSelector,
    pub declarations: Vec<CssDeclaration>,
}

/// Represents the Css selectors
#[derive(Debug, Clone)]
pub struct CssSelector {
    pub selector_type: SelectorType,
    pub value: String,
}

/// Defines the types of Css selectors
#[derive(Debug, Clone)]
pub enum SelectorType {
    Element,
    Class,
    Id,
    Universal,
    Attribute,
    PseudoClass,
    PseudoElement,
    Combinator,
}

/// Represents Css declaration and its variants
#[derive(Debug, Clone)]
pub struct CssDeclaration {
    pub property: String,
    pub value: String,
    pub important: bool,
}

/// Integration of Css AST with HTML AST
pub struct HtmlCssAST {
    pub html: HtmlAST,
    pub css: CssAST,
}

/// Represent the JavaScript AST
#[derive(Debug, Clone)]
pub struct JsAST {
    pub program: Program, 
}

/// Integration of the JsAst into HTML AST
#[derive(Debug, Clone)]
pub struct JsHtmlAST {
    pub html: HtmlAST,  
    pub js: JsAST,      
}

/// Represents a scope in JavaScript with variable declarations
#[derive(Debug, Clone)]
pub struct Scope {
    pub variables: HashMap<String, VariableInfo>,
    pub parent: Option<Box<Scope>>,
}

/// Stores metadata about a declared variable
#[derive(Debug, Clone)]
pub struct VariableInfo {
    pub kind: VariableDeclarationKind,
    pub initialized: bool,
    pub declaration_location: Location,
    pub used: bool,
    pub value_type: ExpressionType,
    pub return_type: Option<ExpressionType>,
}

/// Tracks source code location for error reporting
#[derive(Debug, Clone)]
pub struct Location {
    pub line: usize,
    pub column: usize,
}

#[derive(Debug, Clone)]
pub struct SemanticValidator {
    current_scope: Scope,
    errors: Vec<SemanticError>,
}

#[derive(Debug, Clone)]
pub struct SemanticError {
    message: String,
    location: Option<Location>,
    error_type: ErrorType,
}

#[derive(Debug, Clone)]
pub enum ErrorType {
    UndeclaredVariable,
    UsedBeforeDeclaration,
    TypeMismatch,
}
/// Define a simple type system for basic type checking
#[derive(Debug, Clone, PartialEq)]
pub enum ExpressionType {
    Number,
    String,
    Boolean,
    Object,
    Function,
    Undefined,
    Null,
    Unknown,
}

/// Represent a comprehensive AST of HTML+CSS+JS
#[derive(Debug, Clone)]
pub struct JsHtmlCssAST {
    pub html: HtmlAST,  
    pub css: CssAST,    
    pub js: JsAST,      
}

/// Implement the HTML elements utilities
impl HtmlElement {
    pub fn to_string_pretty(&self) -> String {
        let mut output = String::new();
        self.write_pretty(&mut output, 0).unwrap();
        output
    }

    fn write_pretty(&self, f: &mut String, indent: usize) -> std::fmt::Result {
        let indent_str = "    ".repeat(indent);
        
        // Write opening tag with attributes
        write!(f, "{}<{}", indent_str, self.tag.as_str())?;
        for attr in &self.attributes {
            write!(f, " {}=\"{}\"", attr.name, attr.value)?;
        }

        if self.tag.is_void() {
            writeln!(f, ">")?;
            return Ok(());
        }

        writeln!(f, ">")?;

        // Write children
        for child in &self.children {
            match child {
                HtmlNode::Element(elem) => {
                    elem.write_pretty(f, indent + 1)?;
                }
                HtmlNode::Text(text) => {
                    writeln!(f, "{}{}", "    ".repeat(indent + 1), text)?;
                }
            }
        }

        // Write closing tag - removed the extra \n
        writeln!(f, "{}</{}>", indent_str, self.tag.as_str())?;
        Ok(())
    }
}

/// Implement HTML AST code generation functions and rule enforcement
impl HtmlAST {
    pub fn new() -> Self {

        // Create an empty root HTML element
        let root = HtmlElement {
            tag: HtmlTag::Html,
            attributes: vec![],
            children: vec![],
        };

        HtmlAST {
            root,
            elements: vec![],
        }
    }

    pub fn generate_probabilistic(&mut self, max_depth: usize, max_children: usize) -> HtmlElement {
        let tag_weights = HtmlTagWeight::new(None); // Use default weights
        let mut rng = rand::rng(); // Use rng instead of rng()
        
        // Start with HTML root
        HtmlElement {
            tag: HtmlTag::Html,
            attributes: vec![],
            children: vec![
                // Add required head and body
                self.generate_head(&tag_weights, &mut rng),
                self.generate_body(&tag_weights, &mut rng, max_depth, max_children),
            ],
        }
    }
    
    fn generate_head(&self, weights: &HtmlTagWeight, rng: &mut impl Rng) -> HtmlNode {

        // Basic head with required title
        HtmlNode::Element(HtmlElement {
            tag: HtmlTag::Head,
            attributes: vec![],
            children: vec![
                HtmlNode::Element(HtmlElement {
                    tag: HtmlTag::Title,
                    attributes: vec![],
                    children: vec![HtmlNode::Text("Generated Page".to_string())],
                }),

                // Optionally add meta, link, etc based on weights
                self.maybe_generate_tag(HtmlTag::Meta, weights, rng),
                self.maybe_generate_tag(HtmlTag::Link, weights, rng),
            ].into_iter().filter(|n| !matches!(n, HtmlNode::Element(HtmlElement { tag: HtmlTag::Empty, .. }))).collect(),
        })
    }
    
    fn generate_body(&self, weights: &HtmlTagWeight, rng: &mut impl Rng, depth: usize, max_children: usize) -> HtmlNode {
        let mut body = HtmlElement {
            tag: HtmlTag::Body,
            attributes: vec![],
            children: vec![],
        };
        
        // Generate random number of children
        let num_children = rng.random_range(1..=max_children);
        
        for _ in 0..num_children {
            if depth > 0 {
                body.children.push(self.generate_random_element(weights, rng, depth.saturating_sub(1), max_children));
            }
        }
        
        HtmlNode::Element(body)
    }
    
    fn generate_random_element(&self, weights: &HtmlTagWeight, rng: &mut impl Rng, depth: usize, max_children: usize) -> HtmlNode {
        // Enforce hard depth limit to prevent stack overflow
        if depth == 0 {
            // Return a simple text node at max depth
            return HtmlNode::Text("Generated Content".to_string());
        }
    
        // Create weighted distribution of allowed tags
        let allowed_tags: Vec<HtmlTag> = self.get_allowed_tags_for_depth(depth);
        let tag_weights: Vec<f32> = allowed_tags.iter()
            .map(|tag| weights.get_weight(tag))
            .collect();
            
        let dist = WeightedIndex::new(&tag_weights).unwrap_or_else(|_| {
            // Fallback if weighting fails
            WeightedIndex::new(&[1.0]).unwrap()
        });
        
        let chosen_tag = if allowed_tags.is_empty() {
            &HtmlTag::Div // Safe fallback
        } else {
            &allowed_tags[dist.sample(rng).min(allowed_tags.len() - 1)]
        };
        
        // Don't generate children for void elements
        if chosen_tag.is_void() {
            return HtmlNode::Element(HtmlElement {
                tag: chosen_tag.clone(),
                attributes: self.generate_random_attributes(chosen_tag, rng),
                children: vec![],
            });
        }
        
        // Generate children if not at max depth - with strict limits
        let mut children = vec![];
        if depth > 1 {  // Stricter depth check
            let num_children = rng.random_range(0..=max_children.min(3)); // Stricter child limit
            for _ in 0..num_children {
                children.push(self.generate_random_element(weights, rng, depth.saturating_sub(1), max_children));
            }
        } else if rng.random_bool(0.3) {
            // Add text at near-max depth
            children.push(HtmlNode::Text("Content".to_string()));
        }
        
        HtmlNode::Element(HtmlElement {
            tag: chosen_tag.clone(),
            attributes: self.generate_random_attributes(chosen_tag, rng),
            children,
        })
    }
    
    fn get_allowed_tags_for_depth(&self, depth: usize) -> Vec<HtmlTag> {

        // Return appropriate tags based on depth
        // Lower depths should prefer block elements, higher depths prefer inline
        if depth <= 1 {
            vec![
                HtmlTag::P, HtmlTag::Div, HtmlTag::Section,
                HtmlTag::Article, HtmlTag::Aside, HtmlTag::H1,
                HtmlTag::H2, HtmlTag::H3
            ]
        } else {
            vec![
                HtmlTag::Span, HtmlTag::A, HtmlTag::Strong,
                HtmlTag::Em, HtmlTag::B, HtmlTag::I,
                HtmlTag::Code, HtmlTag::Br
            ]
        }
    }
    
    fn generate_random_attributes(&self, tag: &HtmlTag, rng: &mut impl Rng) -> Vec<HtmlAttribute> {
        let mut attributes = vec![];
        
        // Add tag-specific attributes with some probability
        match tag {
            HtmlTag::A => {
                if rng.random_bool(0.8) {
                    attributes.push(HtmlAttribute {
                        name: "href".to_string(),
                        value: "#".to_string(),
                    });
                }
            }
            HtmlTag::Img => {
                attributes.push(HtmlAttribute {
                    name: "src".to_string(),
                    value: "placeholder.jpg".to_string(),
                });
                attributes.push(HtmlAttribute {
                    name: "alt".to_string(),
                    value: "Generated Image".to_string(),
                });
            }
            _ => {}
        }
        
        // Add generic attributes with low probability
        if rng.random_bool(0.2) {
            attributes.push(HtmlAttribute {
                name: "class".to_string(),
                value: "generated".to_string(),
            });
        }
        
        attributes
    }
    
    fn maybe_generate_tag(&self, tag: HtmlTag, weights: &HtmlTagWeight, rng: &mut impl Rng) -> HtmlNode {
        if rng.random_bool(weights.get_weight(&tag) as f64) {
            HtmlNode::Element(HtmlElement {
                tag: tag.clone(),
                attributes: self.generate_random_attributes(&tag, rng),
                children: vec![],
            })
        } else {
            HtmlNode::Element(HtmlElement {
                tag: HtmlTag::Empty,
                attributes: vec![],
                children: vec![],
            })
        }
    }
}

/// Implement CssAST code generation functions and rule enforcement
impl CssAST {
    pub fn new(browser_target: Option<Browser>) -> Self {
        CssAST {
            rules: Vec::new(),
            browser_target,
        }
    }

    pub fn generate_stylesheet(&mut self, tag_weights: &CssTagWeights) -> String {
        let mut stylesheet = String::new();
        let mut rng = rand::rng(); // Use rng instead of rng()
    
        // Number of rules to generate (randomly between 3-10)
        let rule_count = rng.random_range(3..=10);
        
        for _ in 0..rule_count {
            let rule = self.generate_rule(tag_weights);
            stylesheet.push_str(&rule.to_string());
            stylesheet.push('\n');
        }
    
        stylesheet
    }

    fn generate_rule(&self, tag_weights: &CssTagWeights) -> CssRule {
        let mut rng = rand::rng(); // Use thread_rng instead of rng()
    
        // Generate selector
        let selector = self.generate_selector();
        
        // Generate declarations
        let declaration_count = rng.random_range(1..=5);
        let mut declarations = Vec::with_capacity(declaration_count);
        
        for _ in 0..declaration_count {
            if let Some(decl) = self.generate_declaration(tag_weights) {
                declarations.push(decl);
            }
        }
    
        CssRule {
            selector,
            declarations,
        }
    }

    fn generate_selector(&self) -> CssSelector {
        let mut rng = rand::rng(); // Use thread_rng instead of rng()
        
        // Common HTML elements for element selectors
        const COMMON_ELEMENTS: &[&str] = &[
            "div", "p", "span", "a", "h1", "h2", "h3", 
            "ul", "li", "table", "tr", "td", "input", "button"
        ];
    
        // Common class name components
        const CLASS_COMPONENTS: &[&str] = &[
            "header", "container", "wrapper", "content", "main",
            "sidebar", "footer", "nav", "menu", "button", "card"
        ];
    
        // Generate selector type with weighted probability
        let selector_type = match rng.random_range(0..100) {
            0..=40 => SelectorType::Element,    // 40% element
            41..=70 => SelectorType::Class,     // 30% class
            71..=85 => SelectorType::Id,        // 15% id
            86..=90 => SelectorType::Universal, // 5% universal
            91..=95 => SelectorType::PseudoClass, // 5% pseudo-class
            _ => SelectorType::Attribute,       // 5% attribute
        };
    
        let value = match selector_type {
            SelectorType::Element => {
                COMMON_ELEMENTS.choose(&mut rng).unwrap().to_string()
            },
            SelectorType::Class => {
                format!(".{}", CLASS_COMPONENTS.choose(&mut rng).unwrap())
            },
            SelectorType::Id => {
                format!("#{}", CLASS_COMPONENTS.choose(&mut rng).unwrap())
            },
            SelectorType::Universal => "*".to_string(),
            SelectorType::PseudoClass => {
                let element = COMMON_ELEMENTS.choose(&mut rng).unwrap();
                let pseudo = ["hover", "active", "focus", "first-child", "last-child"]
                    .choose(&mut rng).unwrap();
                format!("{}:{}", element, pseudo)
            },
            SelectorType::Attribute => {
                let element = COMMON_ELEMENTS.choose(&mut rng).unwrap();
                let attr = ["type", "class", "id", "data-*"]
                    .choose(&mut rng).unwrap();
                format!("{}[{}]", element, attr)
            },
            _ => unreachable!()
        };
    
        CssSelector {
            selector_type,
            value,
        }
    }

    fn generate_declaration(&self, weights: &CssTagWeights) -> Option<CssDeclaration> {
        let mut rng = rand::rng();

        // Get a random standard CSS property with weights
        let properties: Vec<&CssProperty> = weights.css_standard_tag_weights.keys().collect();
        if properties.is_empty() {
            return None;
        }

        let property = properties.choose(&mut rng)?;
        let weight = weights.get_standard_weight(property);

        // Only generate if the weight check passes
        if rng.random::<f32>() > weight {
            return None;
        }

        let value = self.generate_property_value(property);
        let important = rng.random_bool(0.1); // 10% chance of !important

        Some(CssDeclaration {
            property: property.as_str().to_string(),
            value,
            important,
        })
    }

    fn generate_property_value(&self, property: &CssProperty) -> String {
        let mut rng = rand::rng();

        match property {
            CssProperty::Color | CssProperty::BackgroundColor => {

                // Generate color in different formats
                match rng.random_range(0..4) {
                    0 => { 
                        
                        // Named color
                        ["red", "blue", "green", "black", "white", "gray"]
                            .choose(&mut rng)
                            .unwrap()
                            .to_string()
                    },
                    1 => { 
                        
                        // Hex color
                        format!("#{:06x}", rng.random_range(0..0x1000000))
                    },
                    2 => { 
                        
                        // RGB
                        format!("rgb({}, {}, {})",
                            rng.random_range(0..256),
                            rng.random_range(0..256),
                            rng.random_range(0..256))
                    },
                    _ => { 
                        
                        // RGBA
                        format!("rgba({}, {}, {}, {:.1})",
                            rng.random_range(0..256),
                            rng.random_range(0..256),
                            rng.random_range(0..256),
                            rng.random_range(0..11) as f32 / 10.0)
                    }
                }
            },
            CssProperty::Width | CssProperty::Height | CssProperty::MaxWidth |
            CssProperty::MaxHeight | CssProperty::MinWidth | CssProperty::MinHeight => {

                // Generate size with units
                match rng.random_range(0..4) {
                    0 => format!("{}px", rng.random_range(1..1001)),
                    1 => format!("{}%", rng.random_range(1..101)),
                    2 => format!("{}rem", rng.random_range(1..11)),
                    _ => format!("{}vh", rng.random_range(1..101)),
                }
            },
            CssProperty::Margin | CssProperty::Padding => {

                // Generate margin/padding with potential shorthand
                match rng.random_range(0..3) {

                    // Single value
                    0 => format!("{}px", rng.random_range(0..51)), 

                    // Two values
                    1 => format!("{}px {}px", 
                        rng.random_range(0..51),
                        rng.random_range(0..51)),

                    // Four values
                    _ => format!("{}px {}px {}px {}px", 
                        rng.random_range(0..51),
                        rng.random_range(0..51),
                        rng.random_range(0..51),
                        rng.random_range(0..51)),
                }
            },
            CssProperty::Display => {
                ["block", "inline", "inline-block", "flex", "grid", "none"]
                    .choose(&mut rng)
                    .unwrap()
                    .to_string()
            },
            CssProperty::Position => {
                ["static", "relative", "absolute", "fixed", "sticky"]
                    .choose(&mut rng)
                    .unwrap()
                    .to_string()
            },
            CssProperty::FontSize => {
                match rng.random_range(0..3) {
                    0 => format!("{}px", rng.random_range(8..73)),
                    1 => format!("{}rem", rng.random_range(1..5)),
                    _ => ["small", "medium", "large", "x-large"]
                        .choose(&mut rng)
                        .unwrap()
                        .to_string(),
                }
            },
            CssProperty::BorderStyle => {
                ["none", "solid", "dashed", "dotted", "double"]
                    .choose(&mut rng)
                    .unwrap()
                    .to_string()
            },
            CssProperty::BorderWidth => {
                format!("{}px", rng.random_range(1..11))
            },
            CssProperty::BorderRadius => {
                format!("{}px", rng.random_range(0..21))
            },
            CssProperty::TextAlign => {
                ["left", "center", "right", "justify"]
                    .choose(&mut rng)
                    .unwrap()
                    .to_string()
            },

            // Default fallback
            _ => String::from("initial"), 
        }
    }

    // Method to validate a CSS rule
    fn validate_rule(&self, rule: &CssRule) -> bool {

        // Validate selector
        if !self.validate_selector(&rule.selector) {
            return false;
        }

        // Validate declarations
        for decl in &rule.declarations {
            if !self.validate_declaration(decl) {
                return false;
            }
        }

        true
    }

    fn validate_selector(&self, selector: &CssSelector) -> bool {

        // Basic selector validation rules
        match selector.selector_type {
            SelectorType::Element => {

                // Validate element name
                let valid_chars = selector.value.chars()
                    .all(|c| c.is_ascii_lowercase() || c == '-');
                valid_chars && !selector.value.is_empty()
            },
            SelectorType::Class => {
                selector.value.starts_with('.') && 
                selector.value.len() > 1 &&
                selector.value[1..].chars()
                    .all(|c| c.is_ascii_alphanumeric() || c == '-' || c == '_')
            },
            SelectorType::Id => {
                selector.value.starts_with('#') &&
                selector.value.len() > 1 &&
                selector.value[1..].chars()
                    .all(|c| c.is_ascii_alphanumeric() || c == '-' || c == '_')
            },
            SelectorType::Universal => {
                selector.value == "*"
            },
            SelectorType::PseudoClass => {
                selector.value.contains(':') &&
                !selector.value.contains("::")
            },
            SelectorType::PseudoElement => {
                selector.value.contains("::")
            },
            SelectorType::Attribute => {
                selector.value.contains('[') &&
                selector.value.contains(']')
            },
            SelectorType::Combinator => {
                selector.value.contains('>') ||
                selector.value.contains('+') ||
                selector.value.contains('~')
            },
        }
    }

    fn validate_declaration(&self, decl: &CssDeclaration) -> bool {

        // Validate property name
        if decl.property.is_empty() || !decl.property.chars()
            .all(|c| c.is_ascii_lowercase() || c == '-') {
            return false;
        }

        // Validate property value
        if decl.value.is_empty() {
            return false;
        }

        // Validate specific property types
        match decl.property.as_str() {
            "color" | "background-color" => self.validate_color(&decl.value),
            "width" | "height" | "margin" | "padding" => self.validate_length(&decl.value),
            "display" => self.validate_display(&decl.value),
            "position" => self.validate_position(&decl.value),
            _ => true, 
        }
    }

    fn validate_color(&self, value: &str) -> bool {

        // Validate different color formats
        value.starts_with('#') && value.len() == 7 || // Hex
        value.starts_with("rgb(") && value.ends_with(')') || // RGB
        value.starts_with("rgba(") && value.ends_with(')') || // RGBA
        ["red", "blue", "green", "black", "white"].contains(&value) // Named colors
    }

    fn validate_length(&self, value: &str) -> bool {

        // Validate length values with units
        if value == "0" {
            return true;
        }

        let units = ["px", "em", "rem", "%", "vh", "vw"];
        units.iter().any(|&unit| {
            value.ends_with(unit) &&
            value[..value.len() - unit.len()]
                .parse::<f32>()
                .is_ok()
        })
    }

    fn validate_display(&self, value: &str) -> bool {
        ["none", "block", "inline", "inline-block", "flex", "grid"]
            .contains(&value)
    }

    fn validate_position(&self, value: &str) -> bool {
        ["static", "relative", "absolute", "fixed", "sticky"]
            .contains(&value)
    }
}

/// Implement writing features for CssRule
impl ToString for CssRule {
    fn to_string(&self) -> String {
        let mut rule = String::new();
        
        // Add selector
        rule.push_str(&self.selector.value);
        rule.push_str(" {\n");
        
        // Add declarations
        for declaration in &self.declarations {
            rule.push_str("    ");
            rule.push_str(&declaration.property);
            rule.push_str(": ");
            rule.push_str(&declaration.value);
            if declaration.important {
                rule.push_str(" !important");
            }
            rule.push_str(";\n");
        }
        
        rule.push_str("}\n");
        rule
    }
}

/// Implement writing features for CssAST
impl ToString for CssAST {
    fn to_string(&self) -> String {
        let mut stylesheet = String::new();
        
        // Add each rule
        for rule in &self.rules {
            stylesheet.push_str(&rule.to_string());
            stylesheet.push('\n');
        }
        
        stylesheet
    }
}

/// Implement HTML+CSS AST 
impl HtmlCssAST {
    pub fn new() -> Self {
        HtmlCssAST {
            html: HtmlAST::new(),
            css: CssAST::new(None),
        }
    }

    pub fn generate_document(&mut self, html_weights: &HtmlTagWeight, css_weights: &CssTagWeights, max_depth: usize, max_children: usize) -> String {
        
        // Generate CSS first
        let css_content = self.css.generate_stylesheet(css_weights);
        
        // Generate HTML with embedded CSS
        let mut html_root = self.html.generate_probabilistic(max_depth, max_children);
        
        // Find the head element or create it if it doesn't exist
        let head = if let Some(HtmlNode::Element(head)) = html_root.children.iter_mut()
            .find(|node| matches!(node, HtmlNode::Element(el) if el.tag == HtmlTag::Head)) {
            head
        } else {
            let head = HtmlElement {
                tag: HtmlTag::Head,
                attributes: vec![],
                children: vec![],
            };
            html_root.children.insert(0, HtmlNode::Element(head));
            if let HtmlNode::Element(ref mut h) = html_root.children[0] {
                h
            } else {
                unreachable!()
            }
        };

        // Add style element with generated CSS
        let style_element = HtmlElement {
            tag: HtmlTag::Style,
            attributes: vec![],
            children: vec![HtmlNode::Text(css_content)],
        };
        head.children.push(HtmlNode::Element(style_element));

        // Convert the HTML tree to string
        format!("<!DOCTYPE html>\n{}", html_root.to_string_pretty())
    }

    // Method to validate both HTML and CSS
    pub fn validate(&self) -> bool {
        
        // Validate all CSS rules
        for rule in &self.css.rules {
            if !self.css.validate_rule(rule) {
                return false;
            }
        }

        // Additional HTML+CSS integration validation
        self.validate_css_selectors_match_html()
    }

    // Validate that CSS selectors match existing HTML elements
    fn validate_css_selectors_match_html(&self) -> bool {
        for rule in &self.css.rules {
            match rule.selector.selector_type {
                SelectorType::Element => {
                    
                    // Check if element exists in HTML
                    if !self.html_contains_element(&rule.selector.value) {
                        return false;
                    }
                },
                SelectorType::Class => {
                    
                    // Check if any element has this class
                    if !self.html_contains_class(&rule.selector.value[1..]) {
                        return false;
                    }
                },
                SelectorType::Id => {
                    
                    // Check if element with ID exists
                    if !self.html_contains_id(&rule.selector.value[1..]) {
                        return false;
                    }
                },

                // Skip other selector types for now
                _ => continue, 
            }
        }
        true
    }

    fn html_contains_element(&self, tag_name: &str) -> bool {
        
        // Recursively check if HTML contains element
        fn check_node(node: &HtmlNode, tag_name: &str) -> bool {
            match node {
                HtmlNode::Element(el) => {
                    if el.tag.as_str() == tag_name {
                        return true;
                    }
                    el.children.iter().any(|child| check_node(child, tag_name))
                },
                _ => false,
            }
        }

        check_node(&HtmlNode::Element(self.html.root.clone()), tag_name)
    }

    fn html_contains_class(&self, class_name: &str) -> bool {
        
        // Recursively check if HTML contains class
        fn check_node(node: &HtmlNode, class_name: &str) -> bool {
            match node {
                HtmlNode::Element(el) => {
                    if el.attributes.iter().any(|attr| {
                        attr.name == "class" && attr.value.split_whitespace().any(|c| c == class_name)
                    }) {
                        return true;
                    }
                    el.children.iter().any(|child| check_node(child, class_name))
                },
                _ => false,
            }
        }

        check_node(&HtmlNode::Element(self.html.root.clone()), class_name)
    }

    fn html_contains_id(&self, id: &str) -> bool {
        
        // Recursively check if HTML contains ID
        fn check_node(node: &HtmlNode, id: &str) -> bool {
            match node {
                HtmlNode::Element(el) => {
                    if el.attributes.iter().any(|attr| attr.name == "id" && attr.value == id) {
                        return true;
                    }
                    el.children.iter().any(|child| check_node(child, id))
                },
                _ => false,
            }
        }

        check_node(&HtmlNode::Element(self.html.root.clone()), id)
    }
}







/// JavaScript AST with weighted probabilistic code generation
/// Semantic enforcement during generation
impl JsAST {

    fn statement_to_string(&self, statement: &Statement) -> String {
        match statement {
            Statement::Empty(_) => ";".to_string(),
            Statement::Block(block) => {
                let mut result = "{\n".to_string();
                for stmt in &block.body {
                    result.push_str(&format!("  {}\n", self.statement_to_string(stmt)));
                }
                result.push_str("}\n");
                result
            },
            Statement::Expression(expr_stmt) => {
                format!("{};", self.expression_to_string(&expr_stmt.expression))
            },
            Statement::VariableDeclaration(var_decl) => {
                let kind = match var_decl.kind {
                    VariableDeclarationKind::Var => "var",
                    VariableDeclarationKind::Let => "let",
                    VariableDeclarationKind::Const => "const",
                };
                
                let declarations: Vec<String> = var_decl.declarations.iter()
                    .map(|decl| {
                        let id = match &decl.id {
                            Pattern::Identifier(ident) => ident.name.clone(),
                            _ => "unknown".to_string(),
                        };
                        
                        if let Some(init) = &decl.init {
                            format!("{} = {}", id, self.expression_to_string(init))
                        } else {
                            id
                        }
                    })
                    .collect();
                
                format!("{} {};", kind, declarations.join(", "))
            },
            Statement::If(if_stmt) => {
                let mut result = format!("if ({}) ", self.expression_to_string(&if_stmt.test));
                result.push_str(&self.statement_to_string(&if_stmt.consequent));
                
                if let Some(alt) = &if_stmt.alternate {
                    result.push_str(&format!("else {}", self.statement_to_string(alt)));
                }
                
                result
            },
            Statement::Return(return_stmt) => {
                if let Some(arg) = &return_stmt.argument {
                    format!("return {};", self.expression_to_string(arg))
                } else {
                    "return;".to_string()
                }
            },
            _ => "/* Unsupported statement type */".to_string(),
        }
    }
    
    fn declaration_to_string(&self, declaration: &Declaration) -> String {
        match declaration {
            Declaration::Variable(var_decl) => self.statement_to_string(&Statement::VariableDeclaration(var_decl.clone())),
            Declaration::Function(func_decl) => {
                // Function ID might be optional for anonymous functions
                let name = match &func_decl.id {
                    Some(id) => id.name.clone(),
                    None => "anonymous".to_string()
                };
                
                let params: Vec<String> = func_decl.params.iter()
                    .map(|param| match param {
                        Pattern::Identifier(ident) => ident.name.clone(),
                        _ => "_".to_string(), // placeholder for complex patterns
                    })
                    .collect();
                
                // Function body is a BlockStatement, not a Statement
                let body_str = {
                    let mut result = "{\n".to_string();
                    for stmt in &func_decl.body.body {
                        result.push_str(&format!("  {}\n", self.statement_to_string(stmt)));
                    }
                    result.push('}');
                    result
                };
                
                format!("function {}({}) {}", name, params.join(", "), body_str)
            },
            Declaration::Class(class_decl) => {
                // Class ID might be optional for anonymous classes
                let name = match &class_decl.id {
                    Some(id) => id.name.clone(),
                    None => "AnonymousClass".to_string()
                };
                format!("class {} {{\n  /* Class body not implemented */\n}}", name)
            },
        }
    }
    
    fn expression_to_string(&self, expression: &Expression) -> String {
        match expression {
            Expression::Identifier(ident) => ident.name.clone(),
            Expression::Literal(literal) => match literal {
                Literal::String(str_lit) => format!("\"{}\"", str_lit.value),
                Literal::Boolean(bool_lit) => bool_lit.value.to_string(),
                Literal::Numeric(num_lit) => num_lit.value.to_string(),
                Literal::Null(_) => "null".to_string(),
                _ => "/* Unsupported literal */".to_string(),
            },
            Expression::Binary(binary_expr) => {
                let left = self.expression_to_string(&binary_expr.left);
                let right = self.expression_to_string(&binary_expr.right);
                let op = match binary_expr.operator {
                    BinaryOperator::Add => "+",
                    BinaryOperator::Subtract => "-",
                    BinaryOperator::Multiply => "*",
                    BinaryOperator::Divide => "/",
                    BinaryOperator::Equal => "==",
                    BinaryOperator::NotEqual => "!=",
                    BinaryOperator::GreaterThan => ">",
                    BinaryOperator::GreaterThanOrEqual => ">=",
                    BinaryOperator::LessThan => "<",
                    BinaryOperator::LessThanOrEqual => "<=",
                    
                    // Placeholder for unknown operators
                    _ => "?", 
                };
                
                format!("({} {} {})", left, op, right)
            },
            Expression::Call(call_expr) => {
                let callee = self.expression_to_string(&call_expr.callee);
                let args: Vec<String> = call_expr.arguments.iter()
                    .map(|arg| match arg {
                        CallArgument::Expression(expr) => self.expression_to_string(expr),
                        _ => "/* Unsupported argument */".to_string(),
                    })
                    .collect();
                
                format!("{}({})", callee, args.join(", "))
            },
            _ => "/* Unsupported expression */".to_string(),
        }
    }

    /// Generate a new JavaScript program respecting semantic rules
    pub fn generate(weight_map: &JsWeightMap, user_depth: Option<usize>) -> Self {
        let mut rng = rand::rng(); // Use thread_rng instead of rng()
        let normal = Normal::new(3.0, 1.5).unwrap();
        
        // Enforce reasonable max depth
        let depth = user_depth.unwrap_or(3).min(5);
        
        // Start with an empty scope
        let root_scope = Scope {
            variables: HashMap::new(),
            parent: None,
        };
    
        let program = if JsAST::get_weight(weight_map, "program_script") > rng.random::<f64>() {
            Program::Script(JsAST::generate_script(weight_map, depth, &mut rng, normal, root_scope))
        } else {
            Program::Module(JsAST::generate_module(weight_map, depth, &mut rng, normal, root_scope))
        };
    
        JsAST { program }
    }

    fn get_weight(weight_map: &JsWeightMap, key: &str) -> f64 {
        weight_map.get_weight(key)
    }

    /// Generate a script with a random number of statements
    fn generate_script(
        weight_map: &JsWeightMap,
        depth: usize,
        rng: &mut impl rand::Rng,
        normal: Normal<f64>,
        scope: Scope,
    ) -> Script {
        // Limit number of statements to prevent stack overflow
        let num_statements = (normal.sample(rng).max(1.0) as usize).min(10);
        let mut body = Vec::new();
        let mut current_scope = scope;
    
        for _ in 0..num_statements {
            let (stmt, updated_scope) = JsAST::generate_statement(weight_map, depth.saturating_sub(1), rng, normal, current_scope.clone());
            body.push(stmt);
            current_scope = updated_scope;
        }
    
        Script {
            directives: vec![],
            body,
        }
    }

    /// Generate a module with random module items
    fn generate_module(
        weight_map: &JsWeightMap,
        depth: usize,
        rng: &mut impl rand::Rng,
        normal: Normal<f64>,
        scope: Scope,
    ) -> Module {
        let num_items = normal.sample(rng).max(1.0) as usize;
        let mut body = Vec::new();
        let mut current_scope = scope;

        for _ in 0..num_items {
            if JsAST::get_weight(weight_map, "module_item_statement") > rng.random::<f64>() {
                let (stmt, updated_scope) = JsAST::generate_statement(weight_map, depth.saturating_sub(1), rng, normal, current_scope.clone());
                body.push(ModuleItem::Statement(stmt));
                current_scope = updated_scope;
            } else if JsAST::get_weight(weight_map, "module_item_import_declaration") > rng.random::<f64>() {
                let (import_decl, updated_scope) = JsAST::generate_import_declaration(rng, current_scope.clone());
                body.push(ModuleItem::ImportDeclaration(import_decl));
                current_scope = updated_scope;
            } else {
                let (export_decl, updated_scope) = JsAST::generate_export_declaration(rng, weight_map, current_scope.clone());
                body.push(ModuleItem::ExportDeclaration(export_decl));
                current_scope = updated_scope;
            }
        }

        Module { body }
    }

    /// Generate a weighted random statement with semantic enforcement
    fn generate_statement(
        weight_map: &JsWeightMap,
        depth: usize,
        rng: &mut impl rand::Rng,
        normal: Normal<f64>,
        scope: Scope,
    ) -> (Statement, Scope) {
        if depth == 0 {
            return (Statement::Empty(EmptyStatement), scope);
        }

        let statement_type = rng.random_range(0..5);
        match statement_type {
            0 if JsAST::get_weight(weight_map, "statement_block") > rng.random::<f64>() => {
                let (block, updated_scope) = JsAST::generate_block_statement(weight_map, depth, rng, normal, scope);
                (Statement::Block(block), updated_scope)
            },
            1 if JsAST::get_weight(weight_map, "statement_expression") > rng.random::<f64>() => {
                let (expr_stmt, scope) = JsAST::generate_expression_statement(weight_map, depth, rng, normal, scope);
                (Statement::Expression(expr_stmt), scope)
            },
            2 if JsAST::get_weight(weight_map, "statement_if") > rng.random::<f64>() => {
                let (if_stmt, scope) = JsAST::generate_if_statement(weight_map, depth, rng, normal, scope);
                (Statement::If(if_stmt), scope)
            },
            3 if JsAST::get_weight(weight_map, "statement_return") > rng.random::<f64>() => {
                let (return_stmt, scope) = JsAST::generate_return_statement(weight_map, depth, rng, normal, scope);
                (Statement::Return(return_stmt), scope)
            },
            _ => {
                let (var_decl, updated_scope) = JsAST::generate_variable_declaration(weight_map, rng, scope);
                (Statement::VariableDeclaration(var_decl), updated_scope)
            },
        }
    }

    /// Generate an expression statement using available variables
    fn generate_expression_statement(
        weight_map: &JsWeightMap,
        depth: usize,
        rng: &mut impl rand::Rng,
        normal: Normal<f64>,
        scope: Scope,
    ) -> (ExpressionStatement, Scope) {
        let (expr, _) = JsAST::generate_expression(weight_map, depth, rng, normal, &scope);
        (ExpressionStatement { expression: expr }, scope)
    }

    /// Generate a semantically valid expression
    fn generate_expression(
        weight_map: &JsWeightMap,
        depth: usize,
        rng: &mut impl rand::Rng,
        normal: Normal<f64>,
        scope: &Scope,
    ) -> (Expression, ExpressionType) {
        if depth == 0 {
            let (expr, expr_type) = JsAST::generate_literal_expression(rng);
            return (expr, expr_type);
        }

        let expression_type = rng.random_range(0..4);
        match expression_type {
            0 if JsAST::get_weight(weight_map, "expression_identifier") > rng.random::<f64>() => {
                // Get a valid variable from scope
                if let Some((ident, var_info)) = JsAST::get_random_variable_from_scope(rng, scope) {
                    (Expression::Identifier(ident), JsAST::get_variable_type(&var_info))
                } else {
                    // Fallback to literal if no variables available
                    JsAST::generate_literal_expression(rng)
                }
            },
            1 if JsAST::get_weight(weight_map, "expression_literal") > rng.random::<f64>() => {
                JsAST::generate_literal_expression(rng)
            },
            2 if JsAST::get_weight(weight_map, "expression_binary") > rng.random::<f64>() => {
                JsAST::generate_binary_expression(weight_map, depth.saturating_sub(1), rng, normal, scope)
            },
            _ => {
                JsAST::generate_call_expression(weight_map, depth.saturating_sub(1), rng, normal, scope)
            },
        }
    }

    /// Get a random variable from the current scope or parent scopes
    fn get_random_variable_from_scope(
        rng: &mut impl rand::Rng,
        scope: &Scope,
    ) -> Option<(Identifier, VariableInfo)> {
        let mut all_variables = Vec::new();
        let mut current_scope_opt = Some(scope);
        
        // Collect variables from all accessible scopes
        while let Some(current_scope) = current_scope_opt {
            for (name, info) in &current_scope.variables {
                if info.initialized {
                    all_variables.push((name.clone(), info.clone()));
                }
            }
            current_scope_opt = current_scope.parent.as_ref().map(|p| p.as_ref());
        }
        
        if all_variables.is_empty() {
            return None;
        }
        
        // Choose a random variable
        let (name, info) = all_variables.choose(rng)?;
        Some((Identifier { name: name.clone() }, info.clone()))
    }
    
    /// Get the type of a variable based on its initialization
    fn get_variable_type(var_info: &VariableInfo) -> ExpressionType {
        var_info.value_type.clone()
    }

    /// Generate a binary expression with type compatibility
    fn generate_binary_expression(
        weight_map: &JsWeightMap,
        depth: usize,
        rng: &mut impl rand::Rng,
        normal: Normal<f64>,
        scope: &Scope,
    ) -> (Expression, ExpressionType) {
        let operator = JsAST::generate_binary_operator(rng);
        
        // Generate operands based on operator requirements
        let (left_expr, left_type) = match operator {
            BinaryOperator::Add | BinaryOperator::Subtract | 
            BinaryOperator::Multiply | BinaryOperator::Divide => {

                // Need numeric operands
                if depth > 0 {
                    JsAST::generate_typed_expression(weight_map, depth.saturating_sub(1), rng, normal, scope, ExpressionType::Number)
                } else {
                    // Provide a base case when depth is 0
                    (
                        Expression::Literal(Literal::Numeric(NumericLiteral { 
                            value: rng.random_range(0..100) 
                        })),
                        ExpressionType::Number
                    )
                }
            },
            _ => {

                // Can use any type
               if depth > 0 {
                    JsAST::generate_expression(weight_map, depth.saturating_sub(1), rng, normal, scope)
                } else {
                    // Provide a base case for when depth is 0
                    JsAST::generate_literal_expression(rng)
                }
            }
        };
        
        // For comparison operators, ensure compatible right operand
        let (right_expr, _) = match operator {
            BinaryOperator::Equal | BinaryOperator::NotEqual => {
                // Try to match left type for comparisons
                JsAST::generate_typed_expression(weight_map, depth.saturating_sub(1), rng, normal, scope, left_type.clone())
            },
            BinaryOperator::Add if left_type == ExpressionType::String => {
                // String concatenation - right side can be any type
                JsAST::generate_expression(weight_map, depth.saturating_sub(1), rng, normal, scope)
            },
            BinaryOperator::Add | BinaryOperator::Subtract |
            BinaryOperator::Multiply | BinaryOperator::Divide => {
                // Arithmetic operations - need numeric operands
                JsAST::generate_typed_expression(weight_map, depth.saturating_sub(1), rng, normal, scope, ExpressionType::Number)
            },
            _ => JsAST::generate_expression(weight_map, depth.saturating_sub(1), rng, normal, scope)
        };
        
        // Determine result type
        let result_type = match operator {
            BinaryOperator::Add => {
                if left_type == ExpressionType::String {
                    ExpressionType::String
                } else {
                    ExpressionType::Number
                }
            },
            BinaryOperator::Subtract | BinaryOperator::Multiply | BinaryOperator::Divide => ExpressionType::Number,
            BinaryOperator::Equal | BinaryOperator::NotEqual |
            BinaryOperator::GreaterThan | BinaryOperator::GreaterThanOrEqual |
            BinaryOperator::LessThan | BinaryOperator::LessThanOrEqual => ExpressionType::Boolean,
            _ => ExpressionType::Unknown
        };
        
        (Expression::Binary(BinaryExpression {
            left: Box::new(left_expr),
            right: Box::new(right_expr),
            operator,
        }), result_type)
    }
    
    /// Generate an expression of a specific type
    fn generate_typed_expression(
        weight_map: &JsWeightMap,
        depth: usize,
        rng: &mut impl rand::Rng,
        normal: Normal<f64>,
        scope: &Scope,
        desired_type: ExpressionType,
    ) -> (Expression, ExpressionType) {
        // Try to find a variable of the desired type
        if let Some((ident, var_info)) = JsAST::find_variable_by_type(scope, &desired_type) {
            return (Expression::Identifier(ident), desired_type);
        }
        
        // Generate a literal of the desired type
        match desired_type {
            ExpressionType::Number => (
                Expression::Literal(Literal::Numeric(NumericLiteral { 
                    value: rng.random_range(0..100) 
                })),
                ExpressionType::Number
            ),
            ExpressionType::String => (
                Expression::Literal(Literal::String(StringLiteral { 
                    value: format!("str_{}", rng.random_range(0..100)) 
                })),
                ExpressionType::String
            ),
            ExpressionType::Boolean => (
                Expression::Literal(Literal::Boolean(BooleanLiteral { 
                    value: rng.random_bool(0.5) 
                })),
                ExpressionType::Boolean
            ),
            _ => JsAST::generate_literal_expression(rng)
        }
    }
    
    /// Find a variable of a specific type in the scope chain
    fn find_variable_by_type(
        scope: &Scope,
        desired_type: &ExpressionType
    ) -> Option<(Identifier, VariableInfo)> {
        let mut current_scope_opt = Some(scope);
        
        while let Some(current_scope) = current_scope_opt {
            for (name, info) in &current_scope.variables {
                if info.initialized && &info.value_type == desired_type {
                    return Some((Identifier { name: name.clone() }, info.clone()));
                }
            }
            current_scope_opt = current_scope.parent.as_ref().map(|p| p.as_ref());
        }
        
        None
    }

    /// Generate a random binary operator
    fn generate_binary_operator(rng: &mut impl rand::Rng) -> BinaryOperator {
        [
            BinaryOperator::Add,
            BinaryOperator::Subtract,
            BinaryOperator::Multiply,
            BinaryOperator::Divide,
            BinaryOperator::Equal,
            BinaryOperator::NotEqual,
            BinaryOperator::GreaterThan,
            BinaryOperator::GreaterThanOrEqual,
            BinaryOperator::LessThan,
            BinaryOperator::LessThanOrEqual,
        ]
        .choose(rng)
        .unwrap()
        .clone() 
    }

    /// Generate a function call using declared variables
    fn generate_call_expression(
        weight_map: &JsWeightMap,
        depth: usize,
        rng: &mut impl rand::Rng,
        normal: Normal<f64>,
        scope: &Scope,
    ) -> (Expression, ExpressionType) {
        // Generate callee - either a known function from scope or a built-in function
        let (callee, return_type) = if let Some((ident, var_info)) = JsAST::find_variable_by_type(scope, &ExpressionType::Function) {
            (Box::new(Expression::Identifier(ident)), var_info.return_type.unwrap_or(ExpressionType::Unknown))
        } else {
            // Use built-in function
            let built_in = JsAST::get_builtin_function(rng);
            (Box::new(Expression::Identifier(Identifier { name: built_in.0.to_string() })), built_in.1)
        };
        
        // Generate arguments
        let num_args = rng.random_range(0..3);
        let mut arguments = Vec::with_capacity(num_args);
        for _ in 0..num_args {
            let (expr, _) = JsAST::generate_expression(weight_map, depth.saturating_sub(1), rng, normal, scope);
            arguments.push(CallArgument::Expression(expr));
        }
        
        (Expression::Call(CallExpression {
            callee,
            arguments,
            optional: false,
        }), return_type)
    }
    
    /// Get a built-in function with its return type
    fn get_builtin_function(rng: &mut impl rand::Rng) -> (&'static str, ExpressionType) {
        let builtins = [
            ("parseInt", ExpressionType::Number),
            ("parseFloat", ExpressionType::Number),
            ("Math.floor", ExpressionType::Number),
            ("Math.ceil", ExpressionType::Number),
            ("Math.random", ExpressionType::Number),
            ("String", ExpressionType::String),
            ("Boolean", ExpressionType::Boolean),
            ("Array.isArray", ExpressionType::Boolean),
            ("console.log", ExpressionType::Undefined),
        ];
        
        builtins.choose(rng).unwrap().clone()
    }

    /// Generate a literal expression with its type
    fn generate_literal_expression(rng: &mut impl rand::Rng) -> (Expression, ExpressionType) {
        match rng.random_range(0..3) {
            0 => (
                Expression::Literal(Literal::Boolean(BooleanLiteral { 
                    value: rng.random_bool(0.5) 
                })),
                ExpressionType::Boolean
            ),
            1 => (
                Expression::Literal(Literal::Numeric(NumericLiteral { 
                    value: rng.random_range(0..100) 
                })),
                ExpressionType::Number
            ),
            _ => (
                Expression::Literal(Literal::String(StringLiteral { 
                    value: format!("str_{}", rng.random_range(0..100)) 
                })),
                ExpressionType::String
            ),
        }
    }

    /// Generate a variable declaration and update scope
    fn generate_variable_declaration(
        weight_map: &JsWeightMap,
        rng: &mut impl rand::Rng,
        mut scope: Scope,
    ) -> (VariableDeclaration, Scope) {
        let kind = if rng.random_bool(0.5) { 
            VariableDeclarationKind::Let 
        } else { 
            VariableDeclarationKind::Const 
        };
        
        // Generate a unique variable name
        let var_name = format!("var_{}", rng.random_range(0..1000));
        
        // Generate an initialization expression
        let literal_type = rng.random_range(0..3);
        let (init, expr_type) = match literal_type {
            0 => (
                Expression::Literal(Literal::Boolean(BooleanLiteral { value: rng.random_bool(0.5) })),
                ExpressionType::Boolean
            ),
            1 => (
                Expression::Literal(Literal::Numeric(NumericLiteral { value: rng.random_range(0..100) })),
                ExpressionType::Number
            ),
            _ => (
                Expression::Literal(Literal::String(StringLiteral { value: format!("str_{}", rng.random_range(0..100)) })),
                ExpressionType::String
            ),
        };
        
        // Create variable info
        let var_info = VariableInfo {
            kind: kind.clone(),
            initialized: true,
            declaration_location: Location { line: 0, column: 0 },
            used: false,
            value_type: expr_type,
            return_type: None,
        };
        
        // Add to scope
        scope.variables.insert(var_name.clone(), var_info);
        
        (VariableDeclaration {
            kind,
            declarations: vec![VariableDeclarator {
                id: Pattern::Identifier(Identifier { name: var_name }),
                init: Some(init),
            }],
        }, scope)
    }

    /// Generate an import declaration that updates scope
    fn generate_import_declaration(
        rng: &mut impl rand::Rng,
        mut scope: Scope,
    ) -> (ImportDeclaration, Scope) {
        let import_name = format!("imported_{}", rng.random_range(1..100));
        
        // Add imported variable to scope
        let var_info = VariableInfo {
            kind: VariableDeclarationKind::Const,
            initialized: true,
            declaration_location: Location { line: 0, column: 0 },
            used: false,
            value_type: ExpressionType::Unknown,
            return_type: None,
        };
        
        scope.variables.insert(import_name.clone(), var_info);
        
        (ImportDeclaration {
            specifiers: vec![ImportSpecifier::Default(ImportDefaultSpecifier {
                local: Identifier { name: import_name },
            })],
            source: StringLiteral {
                value: format!("'module_{}.js'", rng.random_range(1..10)),
            },
        }, scope)
    }

    /// Generate an export declaration
    fn generate_export_declaration(
        rng: &mut impl rand::Rng,
        weight_map: &JsWeightMap,
        scope: Scope,
    ) -> (ExportDeclaration, Scope) {
        let (var_decl, updated_scope) = JsAST::generate_variable_declaration(weight_map, rng, scope);
        
        (ExportDeclaration::ExportNamed(ExportNamedDeclaration {
            declaration: Some(Box::new(Declaration::Variable(var_decl))),
            specifiers: vec![],
            source: None,
        }), updated_scope)
    }

    /// Generate a block statement with its own scope
    fn generate_block_statement(
        weight_map: &JsWeightMap,
        depth: usize,
        rng: &mut impl rand::Rng,
        normal: Normal<f64>,
        parent_scope: Scope,
    ) -> (BlockStatement, Scope) {
        let num_statements = normal.sample(rng).max(1.0) as usize;
        let mut body = Vec::new();
        
        // Create new block scope with parent reference
        let mut block_scope = Scope {
            variables: HashMap::new(),
            parent: Some(Box::new(parent_scope.clone())),
        };
        
        for _ in 0..num_statements {
            let (stmt, updated_scope) = JsAST::generate_statement(weight_map, depth.saturating_sub(1), rng, normal, block_scope);
            body.push(stmt);
            block_scope = updated_scope;
        }
        
        (BlockStatement { body }, parent_scope)
    }

    /// Generate an if-statement
    fn generate_if_statement(
        weight_map: &JsWeightMap,
        depth: usize,
        rng: &mut impl rand::Rng,
        normal: Normal<f64>,
        scope: Scope,
    ) -> (IfStatement, Scope) {
        
        // Generate a condition expression (preferably boolean)
        let (test, _) = JsAST::generate_typed_expression(weight_map, depth.saturating_sub(1), rng, normal, &scope, ExpressionType::Boolean);
        
        // Generate consequent statement
        let (consequent, _) = JsAST::generate_statement(weight_map, depth.saturating_sub(1), rng, normal, scope.clone());
        
        // Optionally generate alternate statement
        let alternate = if rng.random_bool(0.5) {
            let (alt_stmt, _) = JsAST::generate_statement(weight_map, depth.saturating_sub(1), rng, normal, scope.clone());
            Some(Box::new(alt_stmt))
        } else {
            None
        };
        
        (IfStatement {
            test,
            consequent: Box::new(consequent),
            alternate,
        }, scope)
    }

    /// Generate a return statement
    fn generate_return_statement(
        weight_map: &JsWeightMap,
        depth: usize,
        rng: &mut impl rand::Rng,
        normal: Normal<f64>,
        scope: Scope,
    ) -> (ReturnStatement, Scope) {
        let (expr, _) = JsAST::generate_expression(weight_map, depth.saturating_sub(1), rng, normal, &scope);
        
        (ReturnStatement {
            argument: Some(expr),
        }, scope)
    }
}

/// Implements comprehensive AST JS+CSS+HTML
impl JsHtmlCssAST {
    pub fn new() -> Self {
        JsHtmlCssAST {
            html: HtmlAST::new(),
            css: CssAST::new(None),
            js: JsAST { program: Program::Script(Script { directives: vec![], body: vec![] }) },
        }
    }

    pub fn generate_document(
        &mut self,
        html_weights: &HtmlTagWeight,
        css_weights: &CssTagWeights,
        js_weights: &JsWeightMap,
        max_depth: usize,
        max_children: usize
    ) -> String {
        
        // Generate CSS
        let css_content = self.css.generate_stylesheet(css_weights);
        
        // Generate JS
        let js_ast = JsAST::generate(js_weights, Some(max_depth));
        let js_content = js_ast.to_string();
        
        // Generate HTML with embedded CSS and JS
        let mut html_root = self.html.generate_probabilistic(max_depth, max_children);
        
        // Find or create head element
        let head = if let Some(HtmlNode::Element(head)) = html_root.children.iter_mut()
            .find(|node| matches!(node, HtmlNode::Element(el) if el.tag == HtmlTag::Head)) {
            head
        } else {
            let head = HtmlElement {
                tag: HtmlTag::Head,
                attributes: vec![],
                children: vec![],
            };
            html_root.children.insert(0, HtmlNode::Element(head));
            if let HtmlNode::Element(ref mut h) = html_root.children[0] {
                h
            } else {
                unreachable!()
            }
        };

        // Add style element with generated CSS
        let style_element = HtmlElement {
            tag: HtmlTag::Style,
            attributes: vec![],
            children: vec![HtmlNode::Text(css_content)],
        };
        head.children.push(HtmlNode::Element(style_element));

        // Add script element with generated JS
        let script_element = HtmlElement {
            tag: HtmlTag::Script,
            attributes: vec![],
            children: vec![HtmlNode::Text(js_content)],
        };
        head.children.push(HtmlNode::Element(script_element));

        // Convert the HTML tree to string
        format!("<!DOCTYPE html>\n{}", html_root.to_string_pretty())
    }
}

impl ToString for JsAST {
    fn to_string(&self) -> String {
        match &self.program {
            Program::Script(script) => {
                let mut output = String::new();
                
                // Add directives
                for directive in &script.directives {
                    output.push_str(&format!("\"use {}\";\n", directive.value.value));
                }
                
                if !script.directives.is_empty() {
                    output.push('\n');
                }
                
                // Add statements
                for statement in &script.body {
                    output.push_str(&self.statement_to_string(statement));
                    output.push('\n');
                }
                
                output
            },
            Program::Module(module) => {
                let mut output = String::new();
                for item in &module.body {
                    match item {
                        ModuleItem::ImportDeclaration(import) => {
                            output.push_str("import ");
                            match import.specifiers.first() {
                                Some(ImportSpecifier::Default(default)) => {
                                    output.push_str(&default.local.name);
                                },
                                Some(ImportSpecifier::Named(named)) => {
                                    output.push_str("{ ");
                                    output.push_str(&named.local.name);
                                    output.push_str(" }");
                                },
                                Some(ImportSpecifier::Namespace(ns)) => {
                                    output.push_str("* as ");
                                    output.push_str(&ns.local.name);
                                },
                                None => {}
                            }
                            output.push_str(&format!(" from \"{}\";\n", import.source.value));
                        },
                        ModuleItem::ExportDeclaration(export) => {
                            match export {
                                ExportDeclaration::ExportNamed(named) => {
                                    output.push_str("export ");
                                    if let Some(decl) = &named.declaration {
                                        output.push_str(&self.declaration_to_string(decl));
                                    }
                                },
                                ExportDeclaration::ExportDefault(default) => {
                                    output.push_str("export default ");
                                    output.push_str(&self.declaration_to_string(&default.declaration));
                                    output.push_str(";\n");
                                },
                                ExportDeclaration::ExportAll(all) => {
                                    output.push_str(&format!("export * from \"{}\";\n", all.source.value));
                                },
                                ExportDeclaration::ExportDeclaration(decl) => {
                                    output.push_str("export ");
                                    output.push_str(&self.declaration_to_string(&decl.declaration));
                                    output.push_str(";\n");
                                }
                            }
                        },
                        ModuleItem::Statement(stmt) => {
                            output.push_str(&self.statement_to_string(stmt));
                            output.push('\n');
                        }
                    }
                }
                output
            },
        }
    }
}
