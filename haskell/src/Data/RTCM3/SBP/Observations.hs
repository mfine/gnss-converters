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
  ) where

import BasicPrelude
import Control.Lens
import Data.Bits
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
obsFlags = 0x7

-- | Produce packed obs content from GPS L1 observation.
--
toGpsL1PackedObsContents :: Word8 -> GpsL1Observation -> GpsL1ExtObservation -> Maybe PackedObsContent
toGpsL1PackedObsContents _sat _l1 _l1e =
  return PackedObsContent
    { _packedObsContent_P     = undefined -- TODO
    , _packedObsContent_L     = undefined -- TODO
    , _packedObsContent_D     = obsDoppler
    , _packedObsContent_cn0   = undefined -- TODO
    , _packedObsContent_lock  = undefined -- TODO
    , _packedObsContent_sid   = undefined -- TODO
    , _packedObsContent_flags = obsFlags
    }

-- | Produce packed obs content from GPS L1 + L2 observations.
--
toGpsL2PackedObsContents :: Word8 -> GpsL1Observation -> GpsL1ExtObservation -> GpsL2Observation -> GpsL2ExtObservation -> Maybe PackedObsContent
toGpsL2PackedObsContents _sat _l1 _l1e _l2 _l2e =
  return PackedObsContent
    { _packedObsContent_P     = undefined -- TODO
    , _packedObsContent_L     = undefined -- TODO
    , _packedObsContent_D     = obsDoppler
    , _packedObsContent_cn0   = undefined -- TODO
    , _packedObsContent_lock  = undefined -- TODO
    , _packedObsContent_sid   = undefined -- TODO
    , _packedObsContent_flags = obsFlags
    }

-- | Produce packed obs content from GLONASS L1 observation.
--
toGlonassL1PackedObsContents :: Word8 -> GlonassL1Observation -> GlonassL1ExtObservation -> Maybe PackedObsContent
toGlonassL1PackedObsContents _sat _l1 _l1e =
  return PackedObsContent
    { _packedObsContent_P     = undefined -- TODO
    , _packedObsContent_L     = undefined -- TODO
    , _packedObsContent_D     = obsDoppler
    , _packedObsContent_cn0   = undefined -- TODO
    , _packedObsContent_lock  = undefined -- TODO
    , _packedObsContent_sid   = undefined -- TODO
    , _packedObsContent_flags = obsFlags
    }

-- | Produce packed obs content from GLONASS L1 + L2 observations.
--
toGlonassL2PackedObsContents :: Word8 -> GlonassL1Observation -> GlonassL1ExtObservation -> GlonassL2Observation -> GlonassL2ExtObservation -> Maybe PackedObsContent
toGlonassL2PackedObsContents _sat _l1 _l1e _l2 _l2e =
  return PackedObsContent
    { _packedObsContent_P     = undefined -- TODO
    , _packedObsContent_L     = undefined -- TODO
    , _packedObsContent_D     = obsDoppler
    , _packedObsContent_cn0   = undefined -- TODO
    , _packedObsContent_lock  = undefined -- TODO
    , _packedObsContent_sid   = undefined -- TODO
    , _packedObsContent_flags = obsFlags
    }

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
toPackedObsContent = concatMap $
  (<>) <$> maybeToList . l1PackedObsContents <*> maybeToList . l2PackedObsContents

-- | FromObservations produces gps time and packed obs content.
--
class FromObservations a where
  gpsTimeNano       :: MonadStore e m => a -> m GpsTimeNano
  packedObsContents :: a -> [PackedObsContent]

instance FromObservations Msg1002 where
  gpsTimeNano       = toGpsTimeNano . view msg1002_header
  packedObsContents = toPackedObsContent . view msg1002_observations

instance FromObservations Msg1004 where
  gpsTimeNano       = toGpsTimeNano . view msg1004_header
  packedObsContents = toPackedObsContent . view msg1004_observations

instance FromObservations Msg1010 where
  gpsTimeNano       = toGpsTimeNano . view msg1010_header
  packedObsContents = toPackedObsContent . view msg1010_observations

instance FromObservations Msg1012 where
  gpsTimeNano       = toGpsTimeNano . view msg1012_header
  packedObsContents = toPackedObsContent . view msg1012_observations

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
    hdr <- toObservationHeader (fromIntegral i) (fromIntegral $ length obs) m
    return $ MsgObs hdr obs'
  where
    obs     = chunksOf maxObs $ packedObsContents m
    maxObs  = (maxSize - hdrSize) `div` obsSize
    maxSize = 255
    hdrSize = 11
    obsSize = 17
