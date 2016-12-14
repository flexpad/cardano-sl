-- | Parameters for launching everything.

module Pos.Launcher.Param
       ( LoggingParams (..)
       , BaseParams (..)
       , NodeParams (..)
       ) where

import           Control.TimeWarp.Rpc (NetworkAddress)
import           System.Wlog          (LoggerName)
import           Universum

import           Pos.Crypto           (SecretKey)
import           Pos.DHT              (DHTKey, DHTNode, DHTNodeType)
import           Pos.Types            (Timestamp, Utxo)

-- | Contains all parameters required for hierarchical logger initialization.
data LoggingParams = LoggingParams
    { lpRunnerTag     :: !LoggerName        -- ^ prefix for logger, like "time-slave"
    , lpHandlerPrefix :: !(Maybe FilePath)  -- ^ prefix of path for all logs
    , lpConfigPath    :: !(Maybe FilePath)  -- ^ path to logger configuration
    } deriving (Show)

-- | Contains basic & networking parameters for running node.
data BaseParams = BaseParams
    { bpPort               :: !Word16         -- ^ port to run on
    , bpDHTPeers           :: ![DHTNode]      -- ^ peers passed from CLI
    , bpDHTKeyOrType       :: !(Either DHTKey DHTNodeType)
    , bpDHTExplicitInitial :: !Bool
    , bpLoggingParams      :: !LoggingParams  -- ^ logger parameters
    } deriving (Show)

-- | Contains algorithm specific & storage parameters for Node.
data NodeParams = NodeParams
    { npDbPath      :: !(Maybe FilePath)  -- ^ Path to node's data-base. 'Nothing' means memory-mode.
    , npDbPathM     :: !FilePath          -- ^ Modern path to node's data-base.
    , npRebuildDb   :: !Bool              -- ^ @True@ if data-base should be rebuilt
    , npSystemStart :: !Timestamp         -- ^ System start
    , npSecretKey   :: !SecretKey         -- ^ Secret key of this node
    , npBaseParams  :: !BaseParams        -- ^ See 'BaseParams'
    , npCustomUtxo  :: !(Maybe Utxo)      -- ^ predefined custom utxo
    , npTimeLord    :: !Bool              -- ^ @True@ if node started as time-lord
    , npJLFile      :: !(Maybe FilePath)
    , npMalicious   :: !Bool              -- ^ @True@ if node should emulate malicious actions
    , npMalicious'  :: ![NetworkAddress]  -- ^ List of addresses node should cheat on
    , npPropagation :: !Bool              -- ^ Whether to propagate txs, ssc data, blocks to neighbors
    } deriving (Show)
