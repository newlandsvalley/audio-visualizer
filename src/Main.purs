module Main where

import Prelude

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
import Data.Array (length, range, zip)
import Data.ArrayBuffer.ArrayBuffer (ARRAY_BUFFER)
import Data.ArrayBuffer.Typed (toIntArray)
import Data.ArrayBuffer.Types (Uint8Array)
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.HTTP.Method (Method(..))
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Graphics.Canvas (CANVAS, CanvasElement, Context2D, fillRect, setFillStyle, getContext2D, getCanvasElementById)
import Network.HTTP.Affjax (AJAX, affjax, defaultRequest)
import Partial.Unsafe (unsafePartial)
import Text.Smolder.HTML (button)
import Text.Smolder.HTML.Attributes (style)
import Text.Smolder.Markup (Markup, MarkupM, on, text, (!), (#!))
import Text.Smolder.Renderer.DOM (render)


type AudioNodes =
  { source   :: AudioBufferSourceNode
  , analyser :: AnalyserNode
  , frequencyData :: Uint8Array
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
    _ <- setFftSize 2048 analyser
    _ <- setSmoothingTimeConstant 0.8 analyser
    freqBinCount <- frequencyBinCount analyser
    frequencyData <- createUint8Buffer freqBinCount
    dst <- destination audioCtx
    _ <- connect src gain
    _ <- connect gain analyser
    _ <- connect analyser dst
    pure { source : src
         , analyser : analyser
         , frequencyData : frequencyData
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
    button #! on "click" listener $ text "play"

canvasHeight :: Number
canvasHeight = 360.0

canvasWidth :: Number
canvasWidth = 360.0

-- | draw one vertical bar in the overall frame
drawBar :: ∀ eff. Context2D -> Int -> Tuple Int Int ->  Eff (canvas :: CANVAS | eff) Context2D
drawBar drawCtx maxWidth (Tuple idx val) =
  let
    percent = (toNumber val) / 256.0
    height = canvasHeight * percent
    offset = canvasHeight - height - 1.0
    barWidth = canvasWidth / (toNumber maxWidth)
    hue =  (toNumber idx) / (toNumber maxWidth) * 360.0
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

-- | draw one frame of the visualizer
drawVisualizerFrame :: ∀ eff. Context2D -> Uint8Array -> Eff (canvas :: CANVAS | eff) Unit
drawVisualizerFrame drawCtx uint8Array =
  do
    let
      freqs = toIntArray uint8Array
      len = length freqs
      indices = range 0 len
    traverse_ (drawBar drawCtx len) (zip indices freqs)

-- | continuously update the display
drawVisualizer :: ∀ eff. Context -> Eff (dom :: DOM, canvas :: CANVAS, wau :: WebAudio | eff) Unit
drawVisualizer ctx =
  do
    w <- window
    _ <- getByteFrequencyData ctx.audioNodes.analyser ctx.audioNodes.frequencyData
    _ <- drawVisualizerFrame ctx.drawCtx ctx.audioNodes.frequencyData
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