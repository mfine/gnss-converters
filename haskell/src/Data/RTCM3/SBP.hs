{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module:      Data.RTCM3.SBP
-- Copyright:   Copyright (C) 2016 Swift Navigation, Inc.
-- License:     LGPL-3
-- Maintainer:  Swift Navigation <dev@swiftnav.com>
-- Stability:   experimental
-- Portability: portable
--
-- RTCMv3 to SBP Conversions.

module Data.RTCM3.SBP
  ( l1CSidCode
  , l2CMSidCode
  , l2PSidCode
  , sbpCMinM
  , q32Width
  , toWn
  , mjdEpoch
  , updateGpsTimeNano
  , convert
  , newStore
  , validateIodcIode
  , gpsUriToUra
  ) where

import           BasicPrelude
import           Control.Lens
import           Data.Bits
import qualified Data.HashMap.Strict  as M
import           Data.IORef
import           Data.List.Extra      hiding (concat, map)
import           Data.RTCM3
import           Data.RTCM3.SBP.Types
import           Data.Time
import           Data.Word
import           SwiftNav.SBP

{-# ANN module ("HLint: ignore Redundant if"::String) #-}

--------------------------------------------------------------------------------
-- SBP, GNSS, RTCM constant definitions


-- | Two centimeters
--
-- SBP pseudoranges are in units of 2cm, and there are 50 of those in a meter.
twoCM :: Double
twoCM = 0.02

sbpCMinM :: Double
sbpCMinM = 50

-- | Speed of light (meters/msec)
lightSpeedMMSEC :: Double
lightSpeedMMSEC = 299792.458

-- | Speed of light (meters/sec)
lightSpeedMS :: Double
lightSpeedMS = 299792458.0

-- | L1 GPS center frequency (Hz)
l1frequency :: Double
l1frequency = 1.57542e9

-- | L2 GPS center frequency (Hz)
l2frequency :: Double
l2frequency = 1.22760e9

-- | We only support PRNS 1 - 32
maxSats :: Word8
maxSats = 32

-- | Q32.8 carrier phase representation width
q32Width :: Double
q32Width = 256

-- | RTCM phase range resolution.
--
-- See DF018, pg. 3-19 of the RTCM3 spec
phaseRangeRes :: Double
phaseRangeRes = 0.0005

-- | SBP L1 GNSS signal value
--
-- See: https://github.com/swift-nav/libswiftnav/blob/master/include/libswiftnav/signal.h#L65
l1CSidCode :: Word8
l1CSidCode = 0

-- | SBP L2CM GNSS signal value
--
-- See: https://github.com/swift-nav/libswiftnav/blob/master/include/libswiftnav/signal.h#L65
l2CMSidCode :: Word8
l2CMSidCode = 1

-- | SBP L1P and L2P GNSS signal values
--
-- Note that libswiftnav currently does not support L1P or L2P observations,
-- just L1C and L2C. This is a stop gap definition so that we can properly
-- serialize out SBP observations with L1P and L2P observations from external
-- receivers.
l1PSidCode :: Word8
l1PSidCode = 5

l2PSidCode :: Word8
l2PSidCode = 6

-- | L2C code indicator value
--
-- See DF016, pg. 3-17 of the RTCM3 spec
codeIndicator_L2C :: Word8
codeIndicator_L2C = 0

-- | L2P code direct indicator value
--
-- See DF016, pg. 3-17 of the RTCM3 spec
codeIndicator_L2P :: Word8
codeIndicator_L2P = 1

-- | L2P code cross-correlated indicator value
--
-- See DF016, pg. 3-17 of the RTCM3 spec
codeIndicator_L2D :: Word8
codeIndicator_L2D = 2

-- | L2P code correlated indicator value
--
-- See DF016, pg. 3-17 of the RTCM3 spec
codeIndicator_L2W :: Word8
codeIndicator_L2W = 3

-- | Map L2 codes to SBP GnssSignal codes
--
l2codeToSBPSignalCode :: HashMap Word8 Word8
l2codeToSBPSignalCode = M.fromList
  [ (codeIndicator_L2C, l2CMSidCode)
  , (codeIndicator_L2P, l2PSidCode)
  , (codeIndicator_L2D, l2PSidCode)
  , (codeIndicator_L2W, l2PSidCode)
  ]

-- | Maximum number of packed observations to allow in a single SBP message.
--
maxObsPerMessage :: Int
maxObsPerMessage = (maxPayloadSize - headerSize) `div` packedObsSize
  where
    maxPayloadSize = 255
    headerSize     = 11
    packedObsSize  = 17

-- | The official GPS value of Pi. This is the value used by the CS to curve
-- fit ephemeris parameters and should be used in all ephemeris calculations.
gpsPi :: Double
gpsPi = 3.1415926535898

--------------------------------------------------------------------------------
-- General utilities


modifyMap :: (Eq k, Hashable k) => IORef (HashMap k v) -> v -> k -> (v -> v) -> IO v
modifyMap r d k a = do
  m <- readIORef r
  let v = a $ M.lookupDefault d k m
      n = M.insert k v m
  writeIORef r n
  n `seq` return v

applyScaleFactor :: (Floating a, Integral b) => a -> b -> a
applyScaleFactor p x = (2 ** p) * fromIntegral x

--------------------------------------------------------------------------------
-- GNSS RTCM observation reconstruction utilities


fromEcefVal :: Int64 -> Double
fromEcefVal x = fromIntegral x / 10000

newGpsTime :: MonadStore e m => Word32 -> m GpsTimeNano
newGpsTime tow = do
  wn <- view storeWn >>= liftIO . readIORef
  return GpsTimeNano
    { _gpsTimeNano_tow         = tow
    , _gpsTimeNano_ns_residual = 0
    , _gpsTimeNano_wn          = wn
    }

-- | If incoming TOW is less than stored TOW, rollover WN.
--
updateGpsTimeNano :: Word32 -> GpsTimeNano -> GpsTimeNano
updateGpsTimeNano tow gpsTime =
  gpsTime & gpsTimeNano_tow .~ tow &
    if tow >= gpsTime ^. gpsTimeNano_tow then id else
      gpsTimeNano_wn %~ (+ 1)

-- | Produce GPS Time from Observation header, handling WN rollover.
--
toGpsTimeNano :: MonadStore e m => GpsObservationHeader -> m GpsTimeNano
toGpsTimeNano hdr = do
  let tow      = hdr ^. gpsObservationHeader_tow
      station  = hdr ^. gpsObservationHeader_station
  gpsTime      <- newGpsTime tow
  gpsTimeMap   <- view storeGpsTimeMap
  liftIO $ modifyMap gpsTimeMap gpsTime station $ updateGpsTimeNano tow

-- | Produce GPS time from RTCM time (week-number % 1024, tow in seconds, scaled 2^4).
-- Stateful but non-side-effecting. The week number is mod-1024 - add in the base
-- wn from the stored wn masked to 10 bits.
toGpsTimeSec :: MonadStore e m => Word16 -> Word16 -> m GpsTimeSec
toGpsTimeSec wn tow = do
  stateWn <- view storeWn >>= liftIO . readIORef
  return GpsTimeSec
    { _gpsTimeSec_tow = 16 * fromIntegral tow
    , _gpsTimeSec_wn  = stateWn `shiftR` 10 `shiftL` 10 + wn
    }

-- | MJD GPS Epoch - First day in GPS week 0. See DF051 of the RTCM3 spec
--
mjdEpoch :: Word16
mjdEpoch = 44244

-- | Convert from MJD to GPS week number
--
-- See DF051 of the RTCM3 spec
toWn :: Word16 -> Word16
toWn mjd = (mjd - mjdEpoch) `div` 7

-- | Determine whether an L1 RTCM observation is invalid.
--
-- See DF011 and DF012 of the RTCM3 spec
invalid_L1 :: GpsL1Observation -> Bool
invalid_L1 l1 =
  l1 ^. gpsL1Observation_pseudorange == 524288 ||
  l1 ^. gpsL1Observation_carrierMinusCode == -524288

-- | Determine whether an L1 + L2 RTCM observation is invalid.
--
-- See DF011, DF012, and DF018 of the RTCM3 spec
invalid_L2 :: GpsL2Observation -> Bool
invalid_L2 l2 =
  l2 ^. gpsL2Observation_pseudorangeDifference == -8192 ||
  l2 ^. gpsL2Observation_carrierMinusCode == -524288

-- | Construct metric pseudorange (meters!) from L1 RTCM observation.
--
-- See DF011, pg. 3-16 of the RTCM3 spec
metricPseudorange :: GpsL1Observation -> GpsL1ExtObservation -> Double
metricPseudorange l1 l1e =
  twoCM * fromIntegral (l1 ^. gpsL1Observation_pseudorange) +
  lightSpeedMMSEC * fromIntegral (l1e ^. gpsL1ExtObservation_ambiguity)

-- | Construct L1 SBP pseudorange for L1 RTCM observation
--
-- See DF011, pg. 3-16 of the RTCM3 spec
toP_L1 :: GpsL1Observation -> GpsL1ExtObservation -> Word32
toP_L1 l1 l1e = round $ sbpCMinM * metricPseudorange l1 l1e

-- | Construct L2 SBP pseudorange from L1/L2 RTCM observations
--
-- See DF017, pg. 3-18 of the RTCM3 spec
toP_L2 :: GpsL1Observation -> GpsL1ExtObservation -> GpsL2Observation -> Word32
toP_L2 l1 l1e l2 = round $ p * sbpCMinM where
  p = metricPseudorange l1 l1e +
      twoCM * fromIntegral (l2 ^. gpsL2Observation_pseudorangeDifference)

-- | Construct SBP L1 GPS carrier phase from L1 RTCM observation
--
-- See DF012, pg. 3-16 of the RTCM3 spec
toL_L1 :: GpsL1Observation -> GpsL1ExtObservation -> CarrierPhase
toL_L1 l1 l1e = CarrierPhase
  { _carrierPhase_i = fromIntegral li'
  , _carrierPhase_f = fromIntegral lf'
  } where
    p = metricPseudorange l1 l1e
    -- Convert to SBP carrier phase representation per
    -- https://github.com/swift-nav/libsbp/blob/master/spec/yaml/swiftnav/sbp/observation.yaml#L39
    lm :: Double
    lm = p + phaseRangeRes * fromIntegral (l1 ^. gpsL1Observation_carrierMinusCode)
    l = lm / (lightSpeedMS / l1frequency)
    li :: Int32
    li = floor l
    lf :: Word16
    lf = round ((l - fromIntegral li) * q32Width)
    li' :: Int32
    li' = if lf == 256 then li + 1 else li
    lf' :: Word8
    lf' = if lf == 256 then 0 else fromIntegral lf
-- | Construct SBP L2 GPS carrier phase from L2 RTCM observation
--
-- See DF018, pg. 3-18 of the RTCM3 spec
toL_L2 :: GpsL1Observation
       -> GpsL1ExtObservation
       -> GpsL2Observation
       -> GpsL2ExtObservation
       -> CarrierPhase
toL_L2 l1 l1e l2 _l2e = CarrierPhase
  { _carrierPhase_i = fromIntegral li
  , _carrierPhase_f = fromIntegral lf
  } where
    p = metricPseudorange l1 l1e
    -- Convert to SBP carrier phase representation per
    -- https://github.com/swift-nav/libsbp/blob/master/spec/yaml/swiftnav/sbp/observation.yaml#L39
    lm :: Double
    lm = p + phaseRangeRes * fromIntegral (l2 ^. gpsL2Observation_carrierMinusCode)
    l = lm / (lightSpeedMS / l2frequency)
    li :: Int32
    li = floor l
    lf :: Word8
    lf = round ((l - fromIntegral li) * q32Width)

toCn0_L1 :: GpsL1ExtObservation -> Word8
toCn0_L1 = (^. gpsL1ExtObservation_cnr)

toCn0_L2 :: GpsL2ExtObservation -> Word8
toCn0_L2 = (^. gpsL2ExtObservation_cnr)

-- | Convert between DF013 and DF019 lock time to DF402 lock time
--
toLock :: Word8 -> Word8
toLock t
  | t' < 32 = 0
  | t' < 64 = 1
  | t' < 128 = 2
  | t' < 256 = 3
  | t' < 512 = 4
  | t' < 1024 = 5
  | t' < 2048 = 6
  | t' < 4096 = 7
  | t' < 8192 = 8
  | t' < 16384 = 9
  | t' < 32768 = 10
  | t' < 65536 = 11
  | t' < 131072 = 12
  | t' < 262144 = 13
  | t' < 524288 = 14
  | otherwise = 15
  where
    t' :: Word32
    t' =
      1000 *
        if t <= 23 then fromIntegral t else
          if t <= 47 then fromIntegral t * 2 - 24 else
            if t <= 71 then fromIntegral t * 4 - 120 else
              if t <= 95 then fromIntegral t * 8 - 408 else
                if t <= 119 then fromIntegral t * 16 - 1176 else
                  if t <= 126 then fromIntegral t * 32 - 3096 else
                    937

-- | Construct sequenced SBP observation header
--
fromGpsObservationHeader :: MonadStore e m
                         => Word8                -- ^ Total messages
                         -> Word8                -- ^ Message in sequence
                         -> GpsObservationHeader -- ^ RTCM observation header
                         -> m ObservationHeader
fromGpsObservationHeader totalMsgs n hdr = do
  t <- toGpsTimeNano hdr
  return ObservationHeader
    { _observationHeader_t     = t
    -- First nibble is the size of the sequence (n), second nibble is the
    -- zero-indexed counter (ith packet of n). See observation header packing
    -- https://github.com/swift-nav/libsbp/blob/master/spec/yaml/swiftnav/sbp/observation.yaml#L63
    , _observationHeader_n_obs = totalMsgs `shiftL` 4 .|. n
    }

-- | Construct sequenced SBP observation header
--
fromGlonassObservationHeader :: MonadStore e m
                             => Word8                    -- ^ Total messages
                             -> Word8                    -- ^ Message in sequence
                             -> GlonassObservationHeader -- ^ RTCM observation header
                             -> m ObservationHeader
fromGlonassObservationHeader _totalMsgs _n _hdr = undefined

-- | Construct an L1 GnssSignal
--
toL1GnssSignal :: Word8 -> GpsL1Observation -> GnssSignal16
toL1GnssSignal sat l1 =
  GnssSignal16
    { _gnssSignal16_sat  = sat
    , _gnssSignal16_code = if l1 ^. gpsL1Observation_code then l1PSidCode else l1CSidCode
    }

-- | Construct an L1 SBP PackedObsContent an RTCM satellite vehicle observation
--
fromGpsL1SatelliteObservation :: Word8               -- ^ Satellite PRN
                              -> GpsL1Observation
                              -> GpsL1ExtObservation
                              -> PackedObsContent
fromGpsL1SatelliteObservation sat l1 l1e =
  -- Checks GPS L1 code indicator for RTCM message 1002.
  -- See DF010, pg. 3-17 of the RTCM3 spec.
  PackedObsContent
    { _packedObsContent_P     = toP_L1 l1 l1e
    , _packedObsContent_L     = toL_L1 l1 l1e
    , _packedObsContent_D     = Doppler 0 0
    , _packedObsContent_cn0   = toCn0_L1 l1e
    , _packedObsContent_lock  = toLock $ l1 ^. gpsL1Observation_lockTime
    , _packedObsContent_sid   = toL1GnssSignal sat l1
    , _packedObsContent_flags = 0x7 -- Doppler Invalid
                                    -- 1/2 cycle phase ambiguity resolved
                                    -- carrier valid
                                    -- pseudorange valid
    }

-- | Construct an L1 SBP PackedObsContent an RTCM satellite vehicle observation
--
fromGlonassL1SatelliteObservation :: Word8                   -- ^ Satellite PRN
                                  -> GlonassL1Observation
                                  -> GlonassL1ExtObservation
                                  -> PackedObsContent
fromGlonassL1SatelliteObservation _sat _l1 _l1e = undefined

-- | Construct an L2 GnssSignal
--
toL2GnssSignal :: Word8 -> GpsL2Observation -> Maybe GnssSignal16
toL2GnssSignal sat l2 = do
  code <- M.lookup (l2 ^. gpsL2Observation_code) l2codeToSBPSignalCode
  return GnssSignal16
    { _gnssSignal16_sat  = fromIntegral sat
    , _gnssSignal16_code = code
    }

-- | Construct an L2 SBP PackedObsContent an RTCM satellite vehicle observation
--
fromGpsL2SatelliteObservation :: Word8                  -- ^ Satellite PRN
                              -> GpsL1Observation
                              -> GpsL1ExtObservation
                              -> GpsL2Observation
                              -> GpsL2ExtObservation
                              -> Maybe PackedObsContent
fromGpsL2SatelliteObservation sat l1 l1e l2 l2e = do
  sid <- toL2GnssSignal sat l2
  return PackedObsContent
    { _packedObsContent_P     = toP_L2 l1 l1e l2
    , _packedObsContent_L     = toL_L2 l1 l1e l2 l2e
    , _packedObsContent_D     = Doppler 0 0
    , _packedObsContent_cn0   = toCn0_L2 l2e
    , _packedObsContent_lock  = toLock $ l2 ^. gpsL2Observation_lockTime
    , _packedObsContent_sid   = sid
    , _packedObsContent_flags = 0x7 -- Doppler Invalid
                                    -- 1/2 cycle phase ambiguity resolved
                                    -- carrier valid
                                    -- pseudorange valid
  }

-- | Construct an L2 SBP PackedObsContent an RTCM satellite vehicle observation
--
fromGlonassL2SatelliteObservation :: Word8                  -- ^ Satellite PRN
                                  -> GlonassL1Observation
                                  -> GlonassL1ExtObservation
                                  -> GlonassL2Observation
                                  -> GlonassL2ExtObservation
                                  -> Maybe PackedObsContent
fromGlonassL2SatelliteObservation _sat _l1 _l1e _l2 _l2e = undefined

-- | Validate IODE/IODC flags. The IODC and IODE flags (least significant bits) should be
-- equal. IODC is Word16 while IODE is Word8, so we shift IODC to drop the most significant bits.
-- We return a 1 if valid, 0 if invalid.
validateIodcIode :: Word16 -> Word8 -> Word8
validateIodcIode iodc iode =
  if iodc' == iode then 1 else 0
  where
    iodc' = fromIntegral $ iodc `shiftL` 8 `shiftR` 8

-- | Construct an EphemerisCommonContent from an RTCM 1019 message.
toGpsEphemerisCommonContent :: MonadStore e m => Msg1019 -> m EphemerisCommonContent
toGpsEphemerisCommonContent m = do
  toe <- toGpsTimeSec (m ^. msg1019_ephemeris ^. gpsEphemeris_wn) (m ^. msg1019_ephemeris ^. gpsEphemeris_toe)
  return EphemerisCommonContent
    { _ephemerisCommonContent_sid = GnssSignal16
      { _gnssSignal16_sat  = m ^. msg1019_header ^. gpsEphemerisHeader_sat
      , _gnssSignal16_code = 0 -- there is an L2P status flag in msg 1019, but I don't think that applies
      }
    , _ephemerisCommonContent_toe          = toe
    , _ephemerisCommonContent_ura          = gpsUriToUra (fromIntegral $ m ^. msg1019_ephemeris ^. gpsEphemeris_svHealth)
    , _ephemerisCommonContent_fit_interval = decodeFitInterval (m ^. msg1019_ephemeris ^. gpsEphemeris_fitInterval) (m ^. msg1019_ephemeris ^. gpsEphemeris_iodc)
    , _ephemerisCommonContent_valid        = validateIodcIode (m ^. msg1019_ephemeris ^. gpsEphemeris_iodc) (m ^. msg1019_ephemeris ^. gpsEphemeris_iode)
    , _ephemerisCommonContent_health_bits  = m ^. msg1019_ephemeris ^. gpsEphemeris_svHealth
    }

-- | Construct SBP GPS observation message (possibly chunked).
--
gpsChunkToMsgObs :: MonadStore e m
                 => GpsObservationHeader -- ^ RTCM observation header
                 -> Word8                -- ^ Total messages
                 -> Word8                -- ^ Message in sequence
                 -> [PackedObsContent]
                 -> m MsgObs
gpsChunkToMsgObs hdr totalMsgs n packed = do
  header <- fromGpsObservationHeader totalMsgs n hdr
  return MsgObs
    { _msgObs_header = header
    , _msgObs_obs    = packed
    }

-- | Construct SBP GLO observation message (possibly chunked).
--
glonassChunkToMsgObs :: MonadStore e m
                     => GlonassObservationHeader -- ^ RTCM observation header
                     -> Word8                -- ^ Total messages
                     -> Word8                -- ^ Message in sequence
                     -> [PackedObsContent]
                     -> m MsgObs
glonassChunkToMsgObs hdr totalMsgs n packed = do
  header <- fromGlonassObservationHeader totalMsgs n hdr
  return MsgObs
    { _msgObs_header = header
    , _msgObs_obs    = packed
    }

-- | 16 bit SBP Sender Id is 12 bit RTCM Station Id with high nibble or'd in
--
toSender :: Word16 -> Word16
toSender = (.|. 0xf000)

-- | Decode SBP 'fitInterval' from RTCM/GPS 'fitIntervalFlag' and IODC.
-- Implementation adapted from libswiftnav/ephemeris.c.
decodeFitInterval :: Bool -> Word16 -> Word32
decodeFitInterval fitInt iodc
  | not fitInt = 4 * 60 * 60
  | iodc >= 240 && iodc <= 247 = 8 * 60 * 60
  | (iodc >= 248 && iodc <= 255) || iodc == 496 = 14 * 60 * 60
  | (iodc >= 497 && iodc <= 503) || (iodc >= 1021 && iodc <= 1023) = 26 * 60 * 60
  | iodc >= 504 && iodc <= 510 = 50 * 60 * 60
  | iodc == 511 || (iodc >= 752 && iodc <= 756) = 74 * 60 * 60
  | iodc == 757 = 98 * 60 * 60
  | otherwise = 6 * 60 * 60

-- | Convert between RTCM/GPS URA ("User Range Accuracy") index to a number in meters.
-- See section 2.5.3, "User Range Accuracy", in the GPS signal specification.
-- Indices 1, 3, and 5 are hard-coded according to spec, and 15 is hard-coded according
-- to SBP/Piksi convention.
gpsUriToUra :: Double -> Double
gpsUriToUra uri
  | uri < 0 = -1
  | uri == 1 = 2.8
  | uri == 3 = 5.7
  | uri == 5 = 11.3
  | uri == 15 = 6144
  | uri <= 6 = 2 ** (1 + (uri / 2))
  | uri > 6 && uri < 15 = 2 ** (uri - 2)
  | otherwise = -1

--------------------------------------------------------------------------------
-- RTCM to SBP conversion utilities: RTCM Msgs. 1002 (L1 RTK), 1004 (L1+L2 RTK),
-- 1005 (antenna position), 1006 (antenna position).


-- | Construct an L1 SBP PackedObsContent from an RTCM Msg 1002.
--
fromObservation1002 :: Observation1002 -> [PackedObsContent]
fromObservation1002 obs
  | sat > maxSats = mempty
  | invalid_L1 l1 = mempty
  | otherwise = [obs1]
  where
    sat  = obs ^. observation1002_sat
    l1   = obs ^. observation1002_l1
    l1e  = obs ^. observation1002_l1e
    obs1 = fromGpsL1SatelliteObservation sat l1 l1e

-- | Convert an RTCM L1 1002 observation into an SBP MsgObs.
--
-- This chunking takes places because the number of observations in a given 1002
-- may very well exceed the maximum SBP supported payload size of 255 bytes.
fromMsg1002 :: MonadStore e m => Msg1002 -> m [MsgObs]
fromMsg1002 m = do
  let hdr       = m ^. msg1002_header
      obs       = concatMap fromObservation1002 $ m ^. msg1002_observations
      chunks    = zip [0..] $ chunksOf maxObsPerMessage obs
      totalMsgs = fromIntegral $ length chunks
  forM chunks $ uncurry $ gpsChunkToMsgObs hdr totalMsgs

-- | Construct an L1/L2 SBP PackedObsContent from an RTCM Msg 1004.
--
fromObservation1004 :: Observation1004 -> [PackedObsContent]
fromObservation1004 obs
  -- Only lower set of PRN numbers (1-32) are supported
  | sat > maxSats = mempty
  | invalid_L1 l1 && invalid_L2 l2 = mempty
  | invalid_L1 l1 = maybeToList obs2
  | invalid_L2 l2 = [obs1]
  | otherwise = obs1 : maybeToList obs2
  where
    sat  = obs ^. observation1004_sat
    l1   = obs ^. observation1004_l1
    l1e  = obs ^. observation1004_l1e
    l2   = obs ^. observation1004_l2
    l2e  = obs ^. observation1004_l2e
    obs1 = fromGpsL1SatelliteObservation sat l1 l1e
    obs2 = fromGpsL2SatelliteObservation sat l1 l1e l2 l2e


-- | Convert an RTCM L1+L2 1004 observation into multiple SBP MsgObs.
--
-- This chunking takes places because the number of observations in a given 1004
-- may very well exceed the maximum SBP supported payload size of 255 bytes.
fromMsg1004 :: MonadStore e m => Msg1004 -> m [MsgObs]
fromMsg1004 m = do
  let hdr       = m ^. msg1004_header
      obs       = concatMap fromObservation1004 $ m ^. msg1004_observations
      chunks    = zip [0..] $ chunksOf maxObsPerMessage obs
      totalMsgs = fromIntegral $ length chunks
  forM chunks $ uncurry $ gpsChunkToMsgObs hdr totalMsgs

-- | Convert an RTCM 1005 antenna reference position message into an SBP
-- MsgBasePosEcef.
fromMsg1005 :: MonadStore e m => Msg1005 -> m MsgBasePosEcef
fromMsg1005 m =
  return MsgBasePosEcef
    { _msgBasePosEcef_x = fromEcefVal $ m ^. msg1005_reference ^. antennaReference_ecef_x
    , _msgBasePosEcef_y = fromEcefVal $ m ^. msg1005_reference ^. antennaReference_ecef_y
    , _msgBasePosEcef_z = fromEcefVal $ m ^. msg1005_reference ^. antennaReference_ecef_z
    }

-- | Convert an RTCM 1006 antenna reference position message into an SBP
-- MsgBasePosEcef.
fromMsg1006 :: MonadStore e m => Msg1006 -> m MsgBasePosEcef
fromMsg1006 m =
  return MsgBasePosEcef
    { _msgBasePosEcef_x = fromEcefVal $ m ^. msg1006_reference ^. antennaReference_ecef_x
    , _msgBasePosEcef_y = fromEcefVal $ m ^. msg1006_reference ^. antennaReference_ecef_y
    , _msgBasePosEcef_z = fromEcefVal $ m ^. msg1006_reference ^. antennaReference_ecef_z
    }

-- | Construct an L1 SBP PackedObsContent from an RTCM Msg 1010.
--
fromObservation1010 :: Observation1010 -> [PackedObsContent]
fromObservation1010 obs = [obs1]
  where
    sat  = obs ^. observation1010_sat
    l1   = obs ^. observation1010_l1
    l1e  = obs ^. observation1010_l1e
    obs1 = fromGlonassL1SatelliteObservation sat l1 l1e

-- | Convert an RTCM L1 1010 observation into an SBP MsgObs.
--
-- This chunking takes places because the number of observations in a given 1010
-- may very well exceed the maximum SBP supported payload size of 255 bytes.
fromMsg1010 :: MonadStore e m => Msg1010 -> m [MsgObs]
fromMsg1010 m = do
  let hdr       = m ^. msg1010_header
      obs       = concatMap fromObservation1010 $ m ^. msg1010_observations
      chunks    = zip [0..] $ chunksOf maxObsPerMessage obs
      totalMsgs = fromIntegral $ length chunks
  forM chunks $ uncurry $ glonassChunkToMsgObs hdr totalMsgs

-- | Construct an L1 SBP PackedObsContent from an RTCM Msg 1012.
--
fromObservation1012 :: Observation1012 -> [PackedObsContent]
fromObservation1012 obs = obs1 : maybeToList obs2
  where
    sat  = obs ^. observation1012_sat
    l1   = obs ^. observation1012_l1
    l1e  = obs ^. observation1012_l1e
    l2   = obs ^. observation1012_l2
    l2e  = obs ^. observation1012_l2e
    obs1 = fromGlonassL1SatelliteObservation sat l1 l1e
    obs2 = fromGlonassL2SatelliteObservation sat l1 l1e l2 l2e


-- | Convert an RTCM L1 1012 observation into an SBP MsgObs.
--
-- This chunking takes places because the number of observations in a given 1012
-- may very well exceed the maximum SBP supported payload size of 255 bytes.
fromMsg1012 :: MonadStore e m => Msg1012 -> m [MsgObs]
fromMsg1012 m = do
  let hdr       = m ^. msg1012_header
      obs       = concatMap fromObservation1012 $ m ^. msg1012_observations
  let chunks    = zip [0..] $ chunksOf maxObsPerMessage obs
      totalMsgs = fromIntegral $ length chunks
  forM chunks $ uncurry $ glonassChunkToMsgObs hdr totalMsgs

-- | Convert an RTCM 1019 GPS ephemeris message into an SBP MsgEphemerisGps.
fromMsg1019 :: MonadStore e m => Msg1019 -> m MsgEphemerisGps
fromMsg1019 m = do
  commonContent <- toGpsEphemerisCommonContent m
  toc           <- toGpsTimeSec (m ^. msg1019_ephemeris ^. gpsEphemeris_wn) (m ^. msg1019_ephemeris ^. gpsEphemeris_toc)
  return MsgEphemerisGps
    { _msgEphemerisGps_common   = commonContent
    , _msgEphemerisGps_tgd      =         applyScaleFactor (-31) $ m ^. msg1019_ephemeris ^. gpsEphemeris_tgd
    , _msgEphemerisGps_c_rs     =         applyScaleFactor (-5)  $ m ^. msg1019_ephemeris ^. gpsEphemeris_c_rs
    , _msgEphemerisGps_c_rc     =         applyScaleFactor (-5)  $ m ^. msg1019_ephemeris ^. gpsEphemeris_c_rc
    , _msgEphemerisGps_c_uc     =         applyScaleFactor (-29) $ m ^. msg1019_ephemeris ^. gpsEphemeris_c_uc
    , _msgEphemerisGps_c_us     =         applyScaleFactor (-29) $ m ^. msg1019_ephemeris ^. gpsEphemeris_c_us
    , _msgEphemerisGps_c_ic     =         applyScaleFactor (-29) $ m ^. msg1019_ephemeris ^. gpsEphemeris_c_ic
    , _msgEphemerisGps_c_is     =         applyScaleFactor (-29) $ m ^. msg1019_ephemeris ^. gpsEphemeris_c_is
    , _msgEphemerisGps_dn       = gpsPi * applyScaleFactor (-43)  (m ^. msg1019_ephemeris ^. gpsEphemeris_dn)
    , _msgEphemerisGps_m0       = gpsPi * applyScaleFactor (-31)  (m ^. msg1019_ephemeris ^. gpsEphemeris_m0)
    , _msgEphemerisGps_ecc      =         applyScaleFactor (-33) $ m ^. msg1019_ephemeris ^. gpsEphemeris_ecc
    , _msgEphemerisGps_sqrta    =         applyScaleFactor (-19) $ m ^. msg1019_ephemeris ^. gpsEphemeris_sqrta
    , _msgEphemerisGps_omega0   = gpsPi * applyScaleFactor (-31)  (m ^. msg1019_ephemeris ^. gpsEphemeris_omega0)
    , _msgEphemerisGps_omegadot = gpsPi * applyScaleFactor (-43)  (m ^. msg1019_ephemeris ^. gpsEphemeris_omegadot)
    , _msgEphemerisGps_w        = gpsPi * applyScaleFactor (-31)  (m ^. msg1019_ephemeris ^. gpsEphemeris_w)
    , _msgEphemerisGps_inc      = gpsPi * applyScaleFactor (-31)  (m ^. msg1019_ephemeris ^. gpsEphemeris_i0)
    , _msgEphemerisGps_inc_dot  = gpsPi * applyScaleFactor (-43)  (m ^. msg1019_ephemeris ^. gpsEphemeris_idot)
    , _msgEphemerisGps_af0      =         applyScaleFactor (-31) $ m ^. msg1019_ephemeris ^. gpsEphemeris_af0
    , _msgEphemerisGps_af1      =         applyScaleFactor (-43) $ m ^. msg1019_ephemeris ^. gpsEphemeris_af1
    , _msgEphemerisGps_af2      =         applyScaleFactor (-55) $ m ^. msg1019_ephemeris ^. gpsEphemeris_af2
    , _msgEphemerisGps_iodc     =                                    m ^. msg1019_ephemeris ^. gpsEphemeris_iodc
    , _msgEphemerisGps_iode     =                                    m ^. msg1019_ephemeris ^. gpsEphemeris_iode
    , _msgEphemerisGps_toc      = toc
    }

-- | Convert an RTCM message into possibly multiple SBP messages.
--
convert :: MonadStore e m => RTCM3Msg -> m [SBPMsg]
convert = \case
  (RTCM3Msg1002 m _rtcm3) -> do
    let sender = m ^. msg1002_header ^. gpsObservationHeader_station
    m' <- fromMsg1002 m
    return $ flip fmap m' $ \x -> SBPMsgObs x $ toSBP x $ toSender sender
  (RTCM3Msg1004 m _rtcm3) -> do
    let sender = m ^. msg1004_header ^. gpsObservationHeader_station
    m' <- fromMsg1004 m
    return $ flip fmap m' $ \x -> SBPMsgObs x $ toSBP x $ toSender sender
  (RTCM3Msg1005 m _rtcm3) -> do
    let sender =  m ^. msg1005_reference ^. antennaReference_station
    m' <- fromMsg1005 m
    return [SBPMsgBasePosEcef m' $ toSBP m' $ toSender sender]
  (RTCM3Msg1006 m _rtcm3) -> do
    let sender = m ^. msg1006_reference ^. antennaReference_station
    m' <- fromMsg1006 m
    return [SBPMsgBasePosEcef m' $ toSBP m' $ toSender sender]
  (RTCM3Msg1010 m _rtcm3) -> do
    let sender = m ^. msg1010_header ^. glonassObservationHeader_station
    m' <- fromMsg1010 m
    return $ flip fmap m' $ \x -> SBPMsgObs x $ toSBP x $ toSender sender
  (RTCM3Msg1012 m _rtcm3) -> do
    let sender = m ^. msg1012_header ^. glonassObservationHeader_station
    m' <- fromMsg1012 m
    return $ flip fmap m' $ \x -> SBPMsgObs x $ toSBP x $ toSender sender
  (RTCM3Msg1013 m _rtcm3) -> do
    wn <- view storeWn
    liftIO $ writeIORef wn $ toWn $ m ^. msg1013_header ^. messageHeader_mjd
    return mempty
  (RTCM3Msg1019 m _rtcm3) -> do
    let sender = 0
    m' <- fromMsg1019 m
    return [SBPMsgEphemerisGps m' $ toSBP m' $ toSender sender]
  _rtcm3Msg -> return mempty

newStore :: IO Store
newStore = do
  day <- utctDay <$> getCurrentTime
  let wn = fromIntegral $ div (diffDays day (fromGregorian 1980 1 6)) 7
  Store <$> newIORef wn <*> newIORef mempty
