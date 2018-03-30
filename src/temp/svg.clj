
(in-ns 'temp.core)

(import org.apache.batik.transcoder.image.ImageTranscoder)
(import java.io.FileInputStream)
(import org.apache.batik.transcoder.image.ImageTranscoder)
(import org.apache.batik.transcoder.TranscoderInput)
(import org.apache.batik.transcoder.TranscoderOutput)
(import org.apache.batik.transcoder.TranscodingHints)
(import org.apache.batik.dom.svg.SVGDOMImplementation)
(import org.apache.batik.util.SVGConstants)

(defn open-svg [filename width height]
  (let [transcoder-hints (new TranscodingHints)
        result (atom nil)]
    (.put transcoder-hints ImageTranscoder/KEY_XML_PARSER_VALIDATING Boolean/FALSE)
    (.put transcoder-hints ImageTranscoder/KEY_DOM_IMPLEMENTATION
          (SVGDOMImplementation/getDOMImplementation))
    (.put transcoder-hints ImageTranscoder/KEY_DOCUMENT_ELEMENT_NAMESPACE_URI
          SVGConstants/SVG_NAMESPACE_URI)
    (.put transcoder-hints ImageTranscoder/KEY_DOCUMENT_ELEMENT "svg")
    (.put transcoder-hints ImageTranscoder/KEY_WIDTH (float width))
    (.put transcoder-hints ImageTranscoder/KEY_HEIGHT (float height))

    (let [input (new TranscoderInput (new FileInputStream (new File filename)))
          t (proxy [ImageTranscoder] []
              (createImage [w h]
                (new BufferedImage w h BufferedImage/TYPE_INT_ARGB))

              (writeImage [image out]
                (reset! result image)))]

      (.setTranscodingHints t transcoder-hints)
      (.transcode t input nil)

      @result)))

