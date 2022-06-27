use crate::AttrType;
pub(crate) const ATTRS: &[(&str, AttrType)] = &[
    ("attributeName", AttrType::AttributeName),
    ("fr", AttrType::AnyAscii),
    ("vector-effect", AttrType::Keyword),
    ("solid-color", AttrType::AnyAscii),
    ("mask-type", AttrType::Keyword),
    ("filterPrimitiveUnits", AttrType::Keyword),
    ("attributeType", AttrType::Keyword),
    ("blend", AttrType::AnyAscii),
    ("animateColor", AttrType::AnyAscii),
    ("animateMotion", AttrType::AnyAscii),
    ("animateTransform", AttrType::AnyAscii),
    ("arabic-form", AttrType::AnyAscii),
    ("bbox", AttrType::AnyAscii),
    ("begin", AttrType::AnyAscii),
    ("by", AttrType::AnyAscii),
    ("color-profile", AttrType::Url),
    ("unicode", AttrType::AnyAscii),
    ("description", AttrType::Text),
    ("dur", AttrType::AnyAscii),
    ("end", AttrType::AnyAscii),
    ("format", AttrType::AnyAscii),
    ("from", AttrType::AnyAscii),
    ("g1", AttrType::AnyAscii),
    ("g2", AttrType::AnyAscii),
    ("glyph-name", AttrType::AnyAscii),
    ("glyphRef", AttrType::AnyAscii),
    ("hidden", AttrType::Keyword),
    ("id", AttrType::Keyword),
    ("in", AttrType::AnyAscii),
    ("in2", AttrType::AnyAscii),
    ("kernelMatrix", AttrType::AnyAscii),
    ("keyPoints", AttrType::AnyAscii),
    ("keySplines", AttrType::AnyAscii),
    ("keyTimes", AttrType::AnyAscii),
    ("line-height", AttrType::AnyAscii),
    ("local", AttrType::AnyAscii),
    ("max", AttrType::AnyAscii),
    ("min", AttrType::AnyAscii),
    ("name", AttrType::AnyAscii),
    ("orient", AttrType::AnyAscii),
    ("orientation", AttrType::AnyAscii),
    ("origin", AttrType::AnyAscii),
    ("panose-1", AttrType::AnyAscii),
    ("path", AttrType::AnyAscii),
    ("repeatCount", AttrType::AnyAscii),
    ("repeatDur", AttrType::AnyAscii),
    ("result", AttrType::AnyAscii),
    ("side", AttrType::Keyword),
    ("string", AttrType::AnyAscii),
    ("tabindex", AttrType::Number),
    ("tableValues", AttrType::AnyAscii),
    ("to", AttrType::UrlFunc), // it could be anything
    ("transform-origin", AttrType::AnyAscii),
    ("u1", AttrType::AnyAscii),
    ("u2", AttrType::AnyAscii),
    ("unicode-range", AttrType::AnyAscii),
    ("values", AttrType::AnyAscii),
    ("widths", AttrType::AnyAscii),

    ("accent-height", AttrType::Number), // Number
    ("accumulate", AttrType::Keyword),
    ("additive", AttrType::Keyword),
    ("alignment-baseline", AttrType::Keyword),
    ("alphabetic", AttrType::Number),       // Number
    ("amplitude", AttrType::Number),        // Number
    ("arcrole", AttrType::Url),             // URI
    ("ascent", AttrType::Number),           // Number
    ("azimuth", AttrType::Number),          // Number
    ("base", AttrType::Url),                // URI
    ("baseFrequency", AttrType::AnyAscii),  // NumberOptionalNumber
    ("baseline-shift", AttrType::AnyAscii), // BaselineShiftValue
    ("baseProfile", AttrType::Text),        // Text
    ("bias", AttrType::Number),             // Number
    ("calcMode", AttrType::Keyword),
    ("cap-height", AttrType::Number),  // Number
    ("class", AttrType::AnyAscii),     // ClassList
    ("clip", AttrType::UrlFunc),       // ClipValue
    ("clip-path", AttrType::UrlFunc),  // ClipPathValue
    ("clip-rule", AttrType::AnyAscii), // ClipFillRule
    ("clipPathUnits", AttrType::Keyword),
    ("color", AttrType::AnyAscii), // Color
    ("color-interpolation", AttrType::Keyword),
    ("color-interpolation-filters", AttrType::Keyword),
    ("color-rendering", AttrType::Keyword),
    ("cursor", AttrType::UrlFunc),         // CursorValue
    ("cx", AttrType::AnyAscii),            // Coordinate
    ("cy", AttrType::AnyAscii),            // Coordinate
    ("d", AttrType::AnyAscii),             // PathData
    ("descent", AttrType::Number),         // Number
    ("diffuseConstant", AttrType::Number), // Number
    ("direction", AttrType::Keyword),
    ("display", AttrType::Keyword),
    ("divisor", AttrType::Number), // Number
    ("dominant-baseline", AttrType::Keyword),
    ("dx", AttrType::AnyAscii), // Lengths
    ("dy", AttrType::AnyAscii), // Lengths
    ("edgeMode", AttrType::Keyword),
    ("elevation", AttrType::Number),                  // Number
    ("enable-background", AttrType::AnyAscii),        // EnableBackgroundValue
    ("exponent", AttrType::Number),                   // Number
    ("externalResourcesRequired", AttrType::Keyword), // Boolean
    ("fill", AttrType::UrlFunc),                      // Paint
    ("fill-opacity", AttrType::AnyAscii),             // OpacityValue
    ("fill-rule", AttrType::AnyAscii),                // ClipFillRule
    ("filter", AttrType::UrlFunc),                    // FilterValue
    ("filterRes", AttrType::AnyAscii),                // NumberOptionalNumber
    ("filterUnits", AttrType::Keyword),
    ("flood-color", AttrType::AnyAscii),      // SVGColor
    ("flood-opacity", AttrType::AnyAscii),    // OpacityValue
    ("font-family", AttrType::AnyAscii),      // FontFamilyValue
    ("font-size", AttrType::AnyAscii),        // FontSizeValue
    ("font-size-adjust", AttrType::AnyAscii), // FontSizeAdjustValue
    ("font-stretch", AttrType::Keyword),
    ("font-style", AttrType::Keyword),
    ("font-variant", AttrType::Keyword),
    ("font-weight", AttrType::Keyword),
    ("fx", AttrType::AnyAscii),                           // Coordinate
    ("fy", AttrType::AnyAscii),                           // Coordinate
    ("glyph-orientation-horizontal", AttrType::AnyAscii), // GlyphOrientationHorizontalValue
    ("glyph-orientation-vertical", AttrType::AnyAscii),   // GlyphOrientationVerticalValue
    ("gradientTransform", AttrType::AnyAscii),            // TransformList
    ("gradientUnits", AttrType::Keyword),
    ("hanging", AttrType::Number),        // Number
    ("height", AttrType::AnyAscii),       // Length
    ("horiz-adv-x", AttrType::Number),    // Number
    ("horiz-origin-x", AttrType::Number), // Number
    ("horiz-origin-y", AttrType::Number), // Number
    ("href", AttrType::Url),              // URI
    ("ideographic", AttrType::Number),    // Number
    ("image-rendering", AttrType::Keyword),
    ("intercept", AttrType::Number),          // Number
    ("k", AttrType::Number),                  // Number
    ("k1", AttrType::Number),                 // Number
    ("k2", AttrType::Number),                 // Number
    ("k3", AttrType::Number),                 // Number
    ("k4", AttrType::Number),                 // Number
    ("kernelUnitLength", AttrType::AnyAscii), // NumberOptionalNumber
    ("kerning", AttrType::AnyAscii),          // KerningValue
    ("lang", AttrType::AnyAscii),             // LanguageCodes
    ("lengthAdjust", AttrType::Keyword),
    ("letter-spacing", AttrType::AnyAscii),  // SpacingValue
    ("lighting-color", AttrType::AnyAscii),  // SVGColor
    ("limitingConeAngle", AttrType::Number), // Number
    ("marker-end", AttrType::UrlFunc),           // MarkerValue
    ("marker-mid", AttrType::UrlFunc),           // MarkerValue
    ("marker-start", AttrType::UrlFunc),         // MarkerValue
    ("markerHeight", AttrType::AnyAscii),    // Length
    ("markerUnits", AttrType::Keyword),
    ("markerWidth", AttrType::AnyAscii), // Length
    ("mask", AttrType::UrlFunc),         // MaskValue
    ("maskContentUnits", AttrType::Keyword),
    ("maskUnits", AttrType::Keyword),
    ("mathematical", AttrType::Number), // Number
    ("media", AttrType::AnyAscii),      // MediaDesc
    ("method", AttrType::Keyword),
    ("mode", AttrType::Keyword),
    ("numOctaves", AttrType::Number), // Integer
    ("offset", AttrType::AnyAscii),   // NumberOrPercentage
    ("opacity", AttrType::AnyAscii),  // OpacityValue
    ("operator", AttrType::Keyword),
    ("order", AttrType::AnyAscii), // NumberOptionalNumber
    ("overflow", AttrType::Keyword),
    ("overline-position", AttrType::Number),  // Number
    ("overline-thickness", AttrType::Number), // Number
    ("pathLength", AttrType::Number),         // Number
    ("patternContentUnits", AttrType::Keyword),
    ("patternTransform", AttrType::AnyAscii), // TransformList
    ("patternUnits", AttrType::Keyword),
    ("pointer-events", AttrType::Keyword),
    ("points", AttrType::AnyAscii),              // Points
    ("pointsAtX", AttrType::Number),             // Number
    ("pointsAtY", AttrType::Number),             // Number
    ("pointsAtZ", AttrType::Number),             // Number
    ("preserveAlpha", AttrType::Keyword),        // Boolean
    ("preserveAspectRatio", AttrType::AnyAscii), // PreserveAspectRatioSpec
    ("primitiveUnits", AttrType::Keyword),
    ("r", AttrType::AnyAscii),      // Length
    ("radius", AttrType::AnyAscii), // NumberOptionalNumber
    ("refX", AttrType::AnyAscii),   // Coordinate
    ("refY", AttrType::AnyAscii),   // Coordinate
    ("rendering-intent", AttrType::Keyword),
    ("requiredExtensions", AttrType::AnyAscii), // ExtensionList
    ("requiredFeatures", AttrType::AnyAscii),   // FeatureList
    ("restart", AttrType::Keyword),
    ("role", AttrType::Url),        // URI
    ("rotate", AttrType::AnyAscii), // Numbers
    ("rx", AttrType::AnyAscii),     // Length
    ("ry", AttrType::AnyAscii),     // Length
    ("scale", AttrType::Number),    // Number
    ("seed", AttrType::Number),     // Number
    ("shape-rendering", AttrType::Keyword),
    ("show", AttrType::Keyword),
    ("slope", AttrType::Number), // Number
    ("space", AttrType::Keyword),
    ("spacing", AttrType::Keyword),
    ("specularConstant", AttrType::Number), // Number
    ("specularExponent", AttrType::Number), // Number
    ("spreadMethod", AttrType::Keyword),
    ("startOffset", AttrType::AnyAscii),  // Length
    ("stdDeviation", AttrType::AnyAscii), // NumberOptionalNumber
    ("stemh", AttrType::Number),          // Number
    ("stemv", AttrType::Number),          // Number
    ("stitchTiles", AttrType::Keyword),
    ("stop-color", AttrType::AnyAscii),            // SVGColor
    ("stop-opacity", AttrType::AnyAscii),          // OpacityValue
    ("strikethrough-position", AttrType::Number),  // Number
    ("strikethrough-thickness", AttrType::Number), // Number
    ("stroke", AttrType::UrlFunc),                 // Paint
    ("stroke-dasharray", AttrType::AnyAscii),      // StrokeDashArrayValue
    ("stroke-dashoffset", AttrType::AnyAscii),     // StrokeDashOffsetValue
    ("stroke-linecap", AttrType::Keyword),
    ("stroke-linejoin", AttrType::Keyword),
    ("stroke-miterlimit", AttrType::AnyAscii), // StrokeMiterLimitValue
    ("stroke-opacity", AttrType::AnyAscii),    // OpacityValue
    ("stroke-width", AttrType::AnyAscii),      // StrokeWidthValue
    ("style", AttrType::StyleSheet),           // StyleSheet
    ("surfaceScale", AttrType::Number),        // Number
    ("systemLanguage", AttrType::AnyAscii),    // LanguageCodes
    ("targetX", AttrType::Number),             // Integer
    ("targetY", AttrType::Number),             // Integer
    ("text-anchor", AttrType::Keyword),
    ("text-decoration", AttrType::AnyAscii), // TextDecorationValue
    ("text-rendering", AttrType::Keyword),
    ("textLength", AttrType::AnyAscii), // Length
    ("title", AttrType::Text),          // Text
    ("transform", AttrType::AnyAscii),  // TransformList
    ("type", AttrType::Keyword),
    ("underline-position", AttrType::Number),  // Number
    ("underline-thickness", AttrType::Number), // Number
    ("unicode-bidi", AttrType::Keyword),
    ("units-per-em", AttrType::Number),   // Number
    ("v-alphabetic", AttrType::Number),   // Number
    ("v-hanging", AttrType::Number),      // Number
    ("v-ideographic", AttrType::Number),  // Number
    ("v-mathematical", AttrType::Number), // Number
    ("version", AttrType::Number),        // Number
    ("vert-adv-y", AttrType::Number),     // Number
    ("vert-origin-x", AttrType::Number),  // Number
    ("vert-origin-y", AttrType::Number),  // Number
    ("viewBox", AttrType::AnyAscii),      // ViewBoxSpec
    ("visibility", AttrType::Keyword),
    ("width", AttrType::AnyAscii),        // Length
    ("word-spacing", AttrType::AnyAscii), // SpacingValue
    ("writing-mode", AttrType::Keyword),
    ("x", AttrType::AnyAscii),      // Coordinates
    ("x-height", AttrType::Number), // Number
    ("x1", AttrType::AnyAscii),     // Coordinate
    ("x2", AttrType::AnyAscii),     // Coordinate
    ("xChannelSelector", AttrType::Keyword),
    ("xmlns", AttrType::Url),   // URI
    ("y", AttrType::AnyAscii),  // Coordinates
    ("y1", AttrType::AnyAscii), // Coordinate
    ("y2", AttrType::AnyAscii), // Coordinate
    ("yChannelSelector", AttrType::Keyword),
    ("z", AttrType::Number), // Number
    ("zoomAndPan", AttrType::Keyword),
];
