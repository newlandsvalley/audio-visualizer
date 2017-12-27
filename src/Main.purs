module Main where

import Prelude (($), (=<<), (/), (*), (+), (-), (<>), Unit, bind, negate, pure, show, unit, void)
import Audio.WebAudio.AudioContext (connect, createBufferSource, currentTime, createGain, createAnalyser, decodeAudioDataAsync, destination, makeAudioContext)
import Audio.WebAudio.Types (AudioContext, AudioBuffer, AudioBufferSourceNode, AnalyserNode, WebAudio)
import Audio.WebAudio.AudioBufferSourceNode (setBuffer, startBufferSource)
import Audio.WebAudio.AnalyserNode
import Audio.WebAudio.Utils (createUint8Buffer)
import Control.Monad.Aff (Aff, launchAff_)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import DOM (DOM)
import DOM.Event.EventTarget (EventListener, eventListener)
import DOM.HTML (window)
import DOM.HTML.Types (htmlDocumentToNonElementParentNode)
import DOM.HTML.Window (document, requestAnimationFrame)
import DOM.Node.NonElementParentNode (getElementById)
import DOM.Node.Types (Element, ElementId(..))
import Data.Array (range, zip)
import Data.ArrayBuffer.ArrayBuffer (ARRAY_BUFFER)
import Data.ArrayBuffer.Typed (toIntArray)
import Data.ArrayBuffer.Types (Uint8Array)
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.HTTP.Method (Method(..))
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Graphics.Canvas (CANVAS, CanvasElement, Context2D, fillRect, setFillStyle, getContext2D, getCanvasElementById)
import Network.HTTP.Affjax (AJAX, affjax, defaultRequest)
import Partial.Unsafe (unsafePartial)
import Text.Smolder.HTML (button, div)
import Text.Smolder.HTML.Attributes (style)
import Text.Smolder.Markup (Markup, on, text, (!), (#!))
import Text.Smolder.Renderer.DOM (render)
import CSS (render, renderedInline) as CSS
import CSS.Geometry (margin, padding)
import CSS.Font (fontSize)
import CSS.Size (px, em)
import CSS.TextAlign (textAlign, center)
import CSS.Background (backgroundColor)
import Color.Scheme.Clrs (blue)


type AudioNodes =
  { source   :: AudioBufferSourceNode
  , analyser :: AnalyserNode
  , frequencyData :: Uint8Array
  , timeDomainData :: Uint8Array
  }

type Context =
  { canvas   :: CanvasElement
  , app      :: Element
  , audioCtx :: AudioContext
  , drawCtx  :: Context2D
  , audioNodes :: AudioNodes
  }


main:: ∀ eff. Eff
         ( ajax :: AJAX
         , wau :: WebAudio
         , canvas :: CANVAS
         , dom :: DOM
         , arrayBuffer :: ARRAY_BUFFER
         | eff
         ) Unit
main =
  launchAff_ configure

configure :: ∀ eff. Aff
         ( ajax :: AJAX
         , wau :: WebAudio
         , canvas :: CANVAS
         , dom :: DOM
         , arrayBuffer :: ARRAY_BUFFER
         | eff
         ) Unit
configure = void $ unsafePartial do
  documentType <- liftEff $ document =<< window
  Just canvas <- liftEff $ getCanvasElementById "canvas"
  Just element <- liftEff $ getElementById (ElementId "app") $ htmlDocumentToNonElementParentNode documentType
  drawCtx <- liftEff $ getContext2D canvas
  audioCtx <- liftEff $ makeAudioContext
  buffer <- loadSoundBuffer audioCtx "mp3/chrono.mp3"
  audioNodes <- liftEff $ configureAudio audioCtx buffer
  let
    context =
        { canvas   : canvas
        , app      : element
        , audioCtx : audioCtx
        , drawCtx  : drawCtx
        , audioNodes : audioNodes
        }
  -- render the play button
  _ <- liftEff $ render element (playButton context)
  pure unit

-- | configure the web-audio graph
configureAudio :: ∀ eff.
     AudioContext
  -> AudioBuffer
  -> Eff (wau :: WebAudio, arrayBuffer :: ARRAY_BUFFER | eff) AudioNodes
configureAudio audioCtx buffer =
  do
    src <- createBufferSource audioCtx
    _ <- setBuffer buffer src
    gain <- createGain audioCtx
    analyser <- createAnalyser audioCtx
    _ <- setMinDecibels (-140.0) analyser
    _ <- setMaxDecibels 0.0 analyser
    _ <- setFftSize sampleSize analyser
    _ <- setSmoothingTimeConstant 0.8 analyser
    freqBinCount <- frequencyBinCount analyser
    frequencyData <- createUint8Buffer freqBinCount
    timeDomainData <- createUint8Buffer freqBinCount
    dst <- destination audioCtx
    _ <- connect src gain
    _ <- connect gain analyser
    _ <- connect analyser dst
    pure { source : src
         , analyser : analyser
         , frequencyData : frequencyData
         , timeDomainData : timeDomainData
         }

-- | set up the 'play' button
playButton :: ∀ eff.
     Context
  -> Markup (EventListener (dom :: DOM, canvas :: CANVAS, wau :: WebAudio | eff ))
playButton ctx =
  let
    listener =
      eventListener (\e -> play ctx)
  in
    div !style divStyle $ do
      button ! style buttonStyle #! on "click" listener $ text "play"

canvasHeight :: Number
canvasHeight = 360.0

canvasWidth :: Number
canvasWidth = 640.0

sampleSize :: Int
sampleSize = 2048

-- | draw one vertical bar in the overall frame of the frequency chart
drawFrequencyBar :: ∀ eff. Context2D -> Tuple Int Int ->  Eff (canvas :: CANVAS | eff) Context2D
drawFrequencyBar drawCtx (Tuple idx val) =
  let
    percent = (toNumber val) / 256.0
    height = canvasHeight * percent
    offset = canvasHeight - height - 1.0
    barWidth = canvasWidth / (toNumber sampleSize)
    hue =  (toNumber idx) / (toNumber sampleSize) * 360.0
    fillStyle = "hsl(" <> (show hue) <> ", 100%, 50%)"
    rectangle =
      { x: (toNumber idx) + barWidth
      , y: offset
      , w: barWidth
      , h: height
      }
    in
      do
        _ <- setFillStyle fillStyle drawCtx
        fillRect drawCtx rectangle

-- | draw one vertical bar in the overall frame of the time domain chart
drawTimeDomainBar :: ∀ eff. Context2D -> Tuple Int Int ->  Eff (canvas :: CANVAS | eff) Context2D
drawTimeDomainBar drawCtx (Tuple idx val) =
  let
    percent = (toNumber val) / 256.0
    height = canvasHeight * percent
    offset = canvasHeight - height - 1.0
    barWidth = canvasWidth / (toNumber sampleSize)
    rectangle =
      { x: (toNumber idx) + barWidth
      , y: offset
      , w: 1.0
      , h: 2.0
      }
    in
      do
        _ <- setFillStyle "white" drawCtx
        fillRect drawCtx rectangle

-- | draw one frame of the visualizer
drawVisualizerFrame :: ∀ eff. Context2D -> Uint8Array -> Uint8Array -> Eff (canvas :: CANVAS | eff) Unit
drawVisualizerFrame drawCtx freqArray timeDomainArray =
  do
    let
      freqs = toIntArray freqArray
      timeDomains = toIntArray timeDomainArray
      indices = range 0 sampleSize
      rectangle =
        { x: 0.0
        , y: 0.0
        , w: canvasWidth
        , h: canvasHeight
        }
    -- grey the entire canvas before we paint
    _ <- setFillStyle "#303030" drawCtx
    _ <- fillRect drawCtx rectangle
    -- draw each individual frequency bar
    _ <- traverse_ (drawFrequencyBar drawCtx) (zip indices freqs)
    -- and each individual time domain bar
    traverse_ (drawTimeDomainBar drawCtx) (zip indices timeDomains)

-- | continuously update the display
drawVisualizer :: ∀ eff. Context -> Eff (dom :: DOM, canvas :: CANVAS, wau :: WebAudio | eff) Unit
drawVisualizer ctx =
  do
    w <- window
    _ <- getByteFrequencyData ctx.audioNodes.analyser ctx.audioNodes.frequencyData
    _ <- getByteTimeDomainData ctx.audioNodes.analyser ctx.audioNodes.timeDomainData
    _ <- drawVisualizerFrame ctx.drawCtx ctx.audioNodes.frequencyData ctx.audioNodes.timeDomainData
    _ <- requestAnimationFrame (drawVisualizer ctx) w
    pure unit

-- | load a single sound buffer resource and decode it
loadSoundBuffer :: ∀ eff.
  AudioContext
  -> String
  -> Aff
     ( ajax :: AJAX
     , wau :: WebAudio
     | eff
     )
     AudioBuffer
loadSoundBuffer ctx fileName = do
  res <- affjax $ defaultRequest { url = fileName, method = Left GET }
  buffer <- decodeAudioDataAsync ctx res.response
  pure buffer

-- | play the tune and display the visualizer
play :: ∀ eff. Context -> Eff (dom :: DOM, canvas :: CANVAS, wau :: WebAudio | eff) Unit
play ctx =
  do
    now <- currentTime ctx.audioCtx
    _ <- startBufferSource now ctx.audioNodes.source
    _ <- drawVisualizer ctx
    pure unit

divStyle :: String
divStyle =
  fromMaybe "" $
    CSS.renderedInline $ CSS.render do
      textAlign center

buttonStyle :: String
buttonStyle =
  fromMaybe "" $
    CSS.renderedInline $ CSS.render do
      _ <- margin (px 10.0) (px 0.0) (px 0.0) (px 0.0)
      _ <- padding (px 5.0) (px 5.0) (px 5.0) (px 5.0)
      _ <- fontSize (em 1.5)
      backgroundColor blue
