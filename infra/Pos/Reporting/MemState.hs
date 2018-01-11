{-# LANGUAGE TypeFamilies #-}

-- | Reporting functionality abstracted.
module Pos.Reporting.MemState
       ( MisbehaviorMetrics (..)
       , ReportingContext (..)
       , HasReportServers (..)
       , HasMisbehaviorMetrics (..)
       , HasLoggerConfig (..)
       , HasReportingContext (..)
       , emptyReportingContext
       , mmRollbacks
       , mmSscFailures
       , mmIgnoredCommitments
       , rcMisbehaviorMetrics
       ) where

import           System.Metrics.Gauge (Gauge)
import           System.Metrics.Counter (Counter)
import           Control.Lens (makeLenses)
import           System.Wlog.LoggerConfig (LoggerConfig)
import           Universum

-- | EKG metric values for misbehaviors
data MisbehaviorMetrics = MisbehaviorMetrics
    { _mmRollbacks :: Gauge
    , _mmSscFailures :: Counter
    , _mmIgnoredCommitments :: Gauge
    }

makeLenses ''MisbehaviorMetrics

-- | Context needed to provide remote reporting capabilities.
data ReportingContext = ReportingContext
    { _rcReportServers :: ![Text] -- ^ Report servers list (urls)
    , _rcLoggingConfig :: !LoggerConfig
    , _rcMisbehaviorMetrics :: Maybe MisbehaviorMetrics
    }

makeLenses ''ReportingContext

class HasReportServers ctx where
    reportServers :: Lens' ctx [Text]

instance HasReportServers ReportingContext where
    reportServers = rcReportServers

class HasLoggerConfig ctx where
    loggerConfig :: Lens' ctx LoggerConfig

instance HasLoggerConfig ReportingContext where
    loggerConfig = rcLoggingConfig

class HasReportingContext ctx where
    reportingContext :: Lens' ctx ReportingContext

class HasMisbehaviorMetrics ctx where
    misbehaviorMetrics :: Lens' ctx (Maybe MisbehaviorMetrics)

instance HasReportingContext ctx => HasMisbehaviorMetrics ctx where
    misbehaviorMetrics = reportingContext . rcMisbehaviorMetrics

emptyReportingContext :: ReportingContext
emptyReportingContext = ReportingContext [] mempty Nothing
