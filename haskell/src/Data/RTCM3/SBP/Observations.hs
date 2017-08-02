{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module:      Data.RTCM3.SBP.Observations
-- Copyright:   Copyright (C) 2016 Swift Navigation, Inc.
-- License:     LGPL-3
-- Maintainer:  Swift Navigation <dev@swiftnav.com>
-- Stability:   experimental
-- Portability: portable
--
-- RTCMv3 to SBP Observation Conversions.

module Data.RTCM3.SBP.Observations
  ( toMsgObs
  , toSender
  , toSBPMsgObs
  ) where

import BasicPrelude
import Control.Lens
import Data.Bits
import Data.Int
import Data.IORef
import Data.List.Extra
import Data.RTCM3
import Data.RTCM3.SBP.Types
import Data.Word
import SwiftNav.SBP

-- | Convert from Glonass epoch to Gps tow.
--
toGpsTow :: MonadStore e m => Word32 -> m Word32
toGpsTow _tow = undefined

-- | FromObservationHeader produces gps tow.
--
class FromObservationHeader a where
  gpsTow :: MonadStore e m => a -> m Word32

instance FromObservationHeader GpsObservationHeader where
  gpsTow = return . view gpsObservationHeader_tow

instance FromObservationHeader GlonassObservationHeader where
  gpsTow = toGpsTow . view glonassObservationHeader_epoch

-- | Produce a week number and handle rollover.
--
toWn :: MonadStore e m => Word32 -> m Word16
toWn _tow = do
  -- TODO handle wn rollover here.
  wn <- view storeWn >>= liftIO . readIORef
  return wn

-- | Convert RTCMv3 observation header to SBP gps time.
--
toGpsTimeNano :: (MonadStore e m, FromObservationHeader a) => a -> m GpsTimeNano
toGpsTimeNano m = do
  tow <- gpsTow m
  wn  <- toWn tow
  return $ GpsTimeNano tow 0 wn

-- | Default observation doppler.
--
obsDoppler :: Doppler
obsDoppler = Doppler 0 0

-- | Default observation flags.
--
obsFlags :: Word8
obsFlags = 7

-- | Calculate pseudorange.
--
pseudorange :: Double -> Word32 -> Word8 -> Double
pseudorange u p amb = 0.02 * fromIntegral p + u * fromIntegral amb

-- | Calculate pseudorange difference.
--
pseudorangeDifference :: Int16 -> Double
pseudorangeDifference d = 0.02 * fromIntegral d

-- | GPS pseudorange unit.
--
gpsPseudorange :: Double
gpsPseudorange = 299792.458

-- | Glonass pseudorange unit.
--
glonassPseudorange :: Double
glonassPseudorange = 599584.916

-- | Convert 2cm units.
--
toP :: Double -> Word32
toP p = round $ 50 * p

-- | Calculate Carrier-phase.
--
carrierPhase :: Double -> Double -> Int32 -> Double
carrierPhase u p d = (p + 0.0005 * fromIntegral d) / 299792458.0 / u

-- | GPS L1 carrier-phase unit.
--
gpsL1CarrierPhase :: Double
gpsL1CarrierPhase = 1.57542e9

-- | GPS L2 carrier-phase unit.
--
gpsL2CarrierPhase :: Double
gpsL2CarrierPhase = 1.22760e9

-- | Glonass L1 carrier-phase unit.
--
glonassL1CarrierPhase :: Word8 -> Double
glonassL1CarrierPhase fcn = 1.602e9 + fromIntegral fcn * 0.5625e6

-- | Glonass L2 carrier-phase unit.
--
glonassL2CarrierPhase :: Word8 -> Double
glonassL2CarrierPhase fcn = 1.246e9 + fromIntegral fcn * 0.4375e6

-- | Convert to carrier phase measurement.
--
toL :: Double -> CarrierPhase
toL l = if f /= 256 then CarrierPhase i (fromIntegral f) else CarrierPhase (i + 1) 0
  where
    i = floor l
    f = (round $ (l - fromIntegral i) * 256) :: Word16

-- | Lock indicator.
--
lock :: Word8 -> Word32
lock t
  |   t <  24 = fromIntegral t
  |   t <  48 = fromIntegral t *  2 -   24
  |   t <  72 = fromIntegral t *  4 -  120
  |   t <  96 = fromIntegral t *  8 -  408
  |   t < 120 = fromIntegral t * 16 - 1176
  |   t < 127 = fromIntegral t * 32 - 3096
  | otherwise = 937

-- | Convert to lock time.
--
toLock :: Word32 -> Word8
toLock t
  | t <     32 = 0
  | t <     64 = 1
  | t <    128 = 2
  | t <    256 = 3
  | t <    512 = 4
  | t <   1024 = 5
  | t <   2048 = 6
  | t <   4096 = 7
  | t <   8192 = 8
  | t <  16384 = 9
  | t <  32768 = 10
  | t <  65536 = 11
  | t < 131072 = 12
  | t < 262144 = 13
  | t < 524288 = 14
  |  otherwise = 15

-- | Map GPS L1 signal.
--
gpsL1Signal :: Word8 -> Bool -> GnssSignal16
gpsL1Signal sat code
  | code      = GnssSignal16 sat 5
  | otherwise = GnssSignal16 sat 0

-- | Map GPS L2 signal.
--
gpsL2Signal :: Word8 -> Word8 -> GnssSignal16
gpsL2Signal sat code
  | code == 0 = GnssSignal16 sat 1
  | otherwise = GnssSignal16 sat 6

-- | Map Glonass L1 signal.
--
glonassL1Signal :: Word8 -> Bool -> Maybe GnssSignal16
glonassL1Signal sat code
  | code      = Just $ GnssSignal16 sat 3
  | otherwise = Nothing

-- | Map Glonass L2 signal.
--
glonassL2Signal :: Word8 -> Word8 -> Maybe GnssSignal16
glonassL2Signal sat code
  | code == 0 = Just $ GnssSignal16 sat 4
  | otherwise = Nothing

-- | Produce packed obs content from GPS L1 observation.
--
toGpsL1PackedObsContents :: Word8 -> GpsL1Observation -> GpsL1ExtObservation -> Maybe PackedObsContent
toGpsL1PackedObsContents sat l1 l1e =
  return PackedObsContent
    { _packedObsContent_P     = toP p
    , _packedObsContent_L     = toL l
    , _packedObsContent_D     = obsDoppler
    , _packedObsContent_cn0   = l1e ^. gpsL1ExtObservation_cnr
    , _packedObsContent_lock  = toLock $ lock (l1 ^. gpsL1Observation_lockTime)
    , _packedObsContent_sid   = gpsL1Signal sat (l1 ^. gpsL1Observation_code)
    , _packedObsContent_flags = obsFlags
    }
  where
    p = pseudorange gpsPseudorange (l1 ^. gpsL1Observation_pseudorange) (l1e ^. gpsL1ExtObservation_ambiguity)
    l = carrierPhase gpsL1CarrierPhase p (l1 ^. gpsL1Observation_carrierMinusCode)

-- | Produce packed obs content from GPS L1 + L2 observations.
--
toGpsL2PackedObsContents :: Word8 -> GpsL1Observation -> GpsL1ExtObservation -> GpsL2Observation -> GpsL2ExtObservation -> Maybe PackedObsContent
toGpsL2PackedObsContents sat l1 l1e l2 l2e =
  return PackedObsContent
    { _packedObsContent_P     = toP p
    , _packedObsContent_L     = toL l
    , _packedObsContent_D     = obsDoppler
    , _packedObsContent_cn0   = l2e ^. gpsL2ExtObservation_cnr
    , _packedObsContent_lock  = toLock $ lock (l2 ^. gpsL2Observation_lockTime)
    , _packedObsContent_sid   = gpsL2Signal sat (l2 ^. gpsL2Observation_code)
    , _packedObsContent_flags = obsFlags
    }
  where
    p = pseudorange gpsPseudorange (l1 ^. gpsL1Observation_pseudorange) (l1e ^. gpsL1ExtObservation_ambiguity) + pseudorangeDifference (l2 ^. gpsL2Observation_pseudorangeDifference)
    l = carrierPhase gpsL2CarrierPhase p (l2 ^. gpsL2Observation_carrierMinusCode)

-- | Produce packed obs content from GLONASS L1 observation.
--
toGlonassL1PackedObsContents :: Word8 -> GlonassL1Observation -> GlonassL1ExtObservation -> Maybe PackedObsContent
toGlonassL1PackedObsContents sat l1 l1e = do
  sid <- glonassL1Signal sat (l1 ^. glonassL1Observation_code)
  return PackedObsContent
    { _packedObsContent_P     = toP p
    , _packedObsContent_L     = toL l
    , _packedObsContent_D     = obsDoppler
    , _packedObsContent_cn0   = l1e ^. glonassL1ExtObservation_cnr
    , _packedObsContent_lock  = toLock $ lock (l1 ^. glonassL1Observation_lockTime)
    , _packedObsContent_sid   = sid
    , _packedObsContent_flags = obsFlags
    }
  where
    p = pseudorange glonassPseudorange (l1 ^. glonassL1Observation_pseudorange) (l1e ^. glonassL1ExtObservation_ambiguity)
    l = carrierPhase (glonassL1CarrierPhase (l1 ^. glonassL1Observation_frequency)) p (l1 ^. glonassL1Observation_carrierMinusCode)

-- | Produce packed obs content from GLONASS L1 + L2 observations.
--
toGlonassL2PackedObsContents :: Word8 -> GlonassL1Observation -> GlonassL1ExtObservation -> GlonassL2Observation -> GlonassL2ExtObservation -> Maybe PackedObsContent
toGlonassL2PackedObsContents sat l1 l1e l2 l2e = do
  sid <- glonassL2Signal sat (l2 ^. glonassL2Observation_code)
  return PackedObsContent
    { _packedObsContent_P     = toP p
    , _packedObsContent_L     = toL l
    , _packedObsContent_D     = obsDoppler
    , _packedObsContent_cn0   = l2e ^. glonassL2ExtObservation_cnr
    , _packedObsContent_lock  = toLock $ lock (l2 ^. glonassL2Observation_lockTime)
    , _packedObsContent_sid   = sid
    , _packedObsContent_flags = obsFlags
    }
  where
    p = pseudorange glonassPseudorange (l1 ^. glonassL1Observation_pseudorange) (l1e ^. glonassL1ExtObservation_ambiguity) + pseudorangeDifference (l2 ^. glonassL2Observation_pseudorangeDifference)
    l = carrierPhase (glonassL2CarrierPhase (l1 ^. glonassL1Observation_frequency)) p (l2 ^. glonassL2Observation_carrierMinusCode)

-- | FromObservation produces l1 and l2 packed obs content.
--
class FromObservation a where
  l1PackedObsContents :: a -> Maybe PackedObsContent
  l2PackedObsContents :: a -> Maybe PackedObsContent

instance FromObservation Observation1002 where
  l1PackedObsContents o = toGpsL1PackedObsContents (o ^. observation1002_sat) (o ^. observation1002_l1) (o ^. observation1002_l1e)
  l2PackedObsContents _o = Nothing

instance FromObservation Observation1004 where
  l1PackedObsContents o = toGpsL1PackedObsContents (o ^. observation1004_sat) (o ^. observation1004_l1) (o ^. observation1004_l1e)
  l2PackedObsContents o = toGpsL2PackedObsContents (o ^. observation1004_sat) (o ^. observation1004_l1) (o ^. observation1004_l1e) (o ^. observation1004_l2) (o ^. observation1004_l2e)

instance FromObservation Observation1010 where
  l1PackedObsContents o = toGlonassL1PackedObsContents (o ^. observation1010_sat) (o ^. observation1010_l1) (o ^. observation1010_l1e)
  l2PackedObsContents _o = Nothing

instance FromObservation Observation1012 where
  l1PackedObsContents o = toGlonassL1PackedObsContents (o ^. observation1012_sat) (o ^. observation1012_l1) (o ^. observation1012_l1e)
  l2PackedObsContents o = toGlonassL2PackedObsContents (o ^. observation1012_sat) (o ^. observation1012_l1) (o ^. observation1012_l1e) (o ^. observation1012_l2) (o ^. observation1012_l2e)

-- | Convert RTCMv3 observation to SBP packed obs contents.
--
toPackedObsContent :: FromObservation a => [a] -> [PackedObsContent]
toPackedObsContent =
  concatMap $ (<>) <$> maybeToList . l1PackedObsContents <*> maybeToList . l2PackedObsContents

-- | FromObservations produces gps time and packed obs content.
--
class FromObservations a where
  gpsTimeNano       :: MonadStore e m => a -> m GpsTimeNano
  packedObsContents :: a -> [PackedObsContent]
  station           :: a -> Word16

instance FromObservations Msg1002 where
  gpsTimeNano       = toGpsTimeNano . view msg1002_header
  packedObsContents = toPackedObsContent . view msg1002_observations
  station           = view $ msg1002_header . gpsObservationHeader_station

instance FromObservations Msg1004 where
  gpsTimeNano       = toGpsTimeNano . view msg1004_header
  packedObsContents = toPackedObsContent . view msg1004_observations
  station           = view $ msg1004_header . gpsObservationHeader_station

instance FromObservations Msg1010 where
  gpsTimeNano       = toGpsTimeNano . view msg1010_header
  packedObsContents = toPackedObsContent . view msg1010_observations
  station           = view $ msg1010_header . glonassObservationHeader_station

instance FromObservations Msg1012 where
  gpsTimeNano       = toGpsTimeNano . view msg1012_header
  packedObsContents = toPackedObsContent . view msg1012_observations
  station           = view $ msg1012_header . glonassObservationHeader_station

-- | Convert RTCMv3 observation messages to SBP observation header.
--
toObservationHeader :: (MonadStore e m, FromObservations a) => Word8 -> Word8 -> a -> m ObservationHeader
toObservationHeader i n m = do
  t <- gpsTimeNano m
  return $ ObservationHeader t $ n `shiftL` 4 .|. i

-- | Convert RTCMv3 observation messages to SBP observation messages.
--
toMsgObs :: (MonadStore e m, FromObservations a) => a -> m [MsgObs]
toMsgObs m =
  iforM obs $ \i obs' -> do
    hdr <- toObservationHeader (fromIntegral i) (fromIntegral n) m
    return $ MsgObs hdr obs'
  where
    n       = length obs
    obs     = chunksOf maxObs $ packedObsContents m
    maxObs  = (maxSize - hdrSize) `div` obsSize
    maxSize = 255
    hdrSize = 11
    obsSize = 17

-- | Get the sender from the station.
--
toSender :: FromObservations a => a -> Word16
toSender = (.|. 61440) . station

-- | Convert RTCMv3 observation messages to SBP observations messages.
--
toSBPMsgObs :: (MonadStore e m, FromObservations a) => a -> m [SBPMsg]
toSBPMsgObs m = do
  ms <- toMsgObs m
  return $ for ms $ \m' ->
    SBPMsgObs m' $ toSBP m' $ toSender m
