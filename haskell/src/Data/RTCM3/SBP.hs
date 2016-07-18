{-# LANGUAGE LambdaCase #-}

-- |
-- Module:      Data.RTCM3.SBP
-- Copyright:   Copyright (C) 2015 Swift Navigation, Inc.
-- License:     LGPL-3
-- Maintainer:  Mark Fine <dev@swiftnav.com>
-- Stability:   experimental
-- Portability: portable
--
-- RTCMv3 to SBP Conversions.

module Data.RTCM3.SBP
  ( convert
  , runConvert
  ) where

import BasicPrelude
import Control.Lens
import Data.Bits
import Data.IORef
import Data.Time
import Data.Word
import Data.RTCM3
import Data.RTCM3.SBP.Types
import SwiftNav.SBP

fromEcefVal :: Int64 -> Double
fromEcefVal x = fromIntegral x / 10000

toGPSTime :: MonadStore e m => GpsObservationHeader -> m ObsGPSTime
toGPSTime hdr = do
  wn  <- view storeWn >>= liftIO . readIORef
  return ObsGPSTime
    { _obsGPSTime_tow = hdr ^. gpsObservationHeader_tow
    , _obsGPSTime_wn  = wn
    }

fromGpsObservationHeader :: MonadStore e m => GpsObservationHeader -> m ObservationHeader
fromGpsObservationHeader hdr = do
  t <- toGPSTime hdr
  return ObservationHeader
    { _observationHeader_t     = t
    , _observationHeader_n_obs = 0x10
    }

toP :: GpsL1Observation -> GpsL1ExtObservation -> Word32
toP l1 l1e = round $ p * 50 where
  p = (0.02 :: Double) * fromIntegral (l1 ^. gpsL1Observation_pseudorange) +
      299792.458 * fromIntegral (l1e ^. gpsL1ExtObservation_ambiguity)

toL :: GpsL1Observation -> GpsL1ExtObservation -> CarrierPhase
toL l1 l1e = CarrierPhase
  { _carrierPhase_i = fromIntegral li
  , _carrierPhase_f = fromIntegral lf
  } where
    p = 0.02 * fromIntegral (l1 ^. gpsL1Observation_pseudorange) +
        299792.458 * fromIntegral (l1e ^. gpsL1ExtObservation_ambiguity)
    lm :: Double
    lm = p + 0.0005 * fromIntegral (l1 ^. gpsL1Observation_carrierMinusCode)
    l = lm / (299792458.0 / 1.57542e9)
    li :: Int32
    li = floor (l)
    lf :: Word8
    lf = truncate ((l - fromIntegral li) * 256)

toCn0 :: GpsL1ExtObservation -> Word8
toCn0 = (^. gpsL1ExtObservation_cnr)

toLock :: GpsL1Observation -> Word16
toLock _l1 = 0

toSid :: Word8 -> GnssSignal
toSid sat = GnssSignal
  { _gnssSignal_sat      = fromIntegral $ sat - 1
  , _gnssSignal_code     = 0
  , _gnssSignal_reserved = 0
  }

fromObservation1002 :: Observation1002 -> Maybe PackedObsContent
fromObservation1002 obs =
  -- Only lower set of PRN numbers (1-32) are supported
  if sat > 32 then Nothing else
    if obs ^. observation1002_l1 ^. gpsL1Observation_code then Nothing else Just PackedObsContent
      { _packedObsContent_P    = toP l1 l1e
      , _packedObsContent_L    = toL l1 l1e
      , _packedObsContent_cn0  = toCn0 l1e
      , _packedObsContent_lock = toLock l1
      , _packedObsContent_sid  = toSid sat
      } where
        sat = obs ^. observation1002_sat
        l1  = obs ^. observation1002_l1
        l1e = obs ^. observation1002_l1e

fromObservation1004 :: Observation1004 -> Maybe PackedObsContent
fromObservation1004 obs =
  -- Only lower set of PRN numbers (1-32) are supported
  if sat > 32 then Nothing else
    if obs ^. observation1004_l1 ^. gpsL1Observation_code then Nothing else Just PackedObsContent
      { _packedObsContent_P    = toP l1 l1e
      , _packedObsContent_L    = toL l1 l1e
      , _packedObsContent_cn0  = toCn0 l1e
      , _packedObsContent_lock = toLock l1
      , _packedObsContent_sid  = toSid sat
      }  where
        sat = obs ^. observation1004_sat
        l1  = obs ^. observation1004_l1
        l1e = obs ^. observation1004_l1e

fromMsg1002 :: MonadStore e m => Msg1002 -> m MsgObs
fromMsg1002 m = do
  header <- fromGpsObservationHeader $ m ^. msg1002_header
  return MsgObs
    { _msgObs_header = header
    , _msgObs_obs    = mapMaybe fromObservation1002 $ m ^. msg1002_observations
    }

fromMsg1004 :: MonadStore e m => Msg1004 -> m MsgObs
fromMsg1004 m = do
  header <- fromGpsObservationHeader $ m ^. msg1004_header
  return MsgObs
    { _msgObs_header = header
    , _msgObs_obs    = mapMaybe fromObservation1004 $ m ^. msg1004_observations
    }

fromMsg1005 :: MonadStore e m => Msg1005 -> m MsgBasePosEcef
fromMsg1005 m =
  return MsgBasePosEcef
    { _msgBasePosEcef_x = fromEcefVal $ m ^. msg1005_reference ^. antennaReference_ecef_x
    , _msgBasePosEcef_y = fromEcefVal $ m ^. msg1005_reference ^. antennaReference_ecef_y
    , _msgBasePosEcef_z = fromEcefVal $ m ^. msg1005_reference ^. antennaReference_ecef_z
    }

fromMsg1006 :: MonadStore e m => Msg1006 -> m MsgBasePosEcef
fromMsg1006 m =
  return MsgBasePosEcef
    { _msgBasePosEcef_x = fromEcefVal $ m ^. msg1006_reference ^. antennaReference_ecef_x
    , _msgBasePosEcef_y = fromEcefVal $ m ^. msg1006_reference ^. antennaReference_ecef_y
    , _msgBasePosEcef_z = fromEcefVal $ m ^. msg1006_reference ^. antennaReference_ecef_z
    }

-- Sender Id is Station Id with high byte or'd in
toSender :: Word16 -> Word16
toSender = (.|. 0xf00)

convert :: MonadStore e m => RTCM3Msg -> m (Maybe SBPMsg)
convert = \case
  (RTCM3Msg1002 m _rtcm3) -> do
    m' <- fromMsg1002 m
    return $ Just $ SBPMsgObs m' $ toSBP m' $
      toSender $ m ^. msg1002_header ^. gpsObservationHeader_station
  (RTCM3Msg1004 m _rtcm3) -> do
    m' <- fromMsg1004 m
    return $ Just $ SBPMsgObs m' $ toSBP m' $
      toSender $ m ^. msg1004_header ^. gpsObservationHeader_station
  (RTCM3Msg1005 m _rtcm3) -> do
    m' <- fromMsg1005 m
    return $ Just $ SBPMsgBasePosEcef m' $ toSBP m' $
      toSender $ m ^. msg1005_reference ^. antennaReference_station
  (RTCM3Msg1006 m _rtcm3) -> do
    m' <- fromMsg1006 m
    return $ Just $ SBPMsgBasePosEcef m' $ toSBP m' $
      toSender $ m ^. msg1006_reference ^. antennaReference_station
  _rtcm3Msg -> return Nothing

toWn :: Day -> Word16
toWn time = fromIntegral $ div (diffDays time (fromGregorian 1980 1 6)) 7

newStore :: IO Store
newStore = do
  day <- utctDay <$> getCurrentTime
  wn  <- newIORef $ toWn day
  return $ Store
    { _storeWn = wn
    }

runConvert :: MonadIO m => ConvertT Store m a -> m a
runConvert m = do
  s <- liftIO $ newStore
  runConvertT s m
