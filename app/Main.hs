{-# OPTIONS_GHC -Wno-unused-imports #-}
module Main (main) where

import OracleNFT qualified 
import Collateral qualified
import PriceFeedOracle qualified
import Minting qualified
import Data.Default (
  def,
 )
import Ply.Plutarch (
  writeTypedScript,
 )
import Data.Text (
  Text,
  pack,
 )
import Plutarch (
  Config (Config),
  TracingMode (DoTracing),
  compile,
 )
import Plutarch.Evaluate (
  evalScript,
 )
import "liqwid-plutarch-extra" Plutarch.Extra.Script (
  applyArguments,
 )
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Lazy qualified as LBS
import Data.Text.Encoding qualified as Text
import Cardano.Binary qualified as CBOR
import Plutarch.Script (Script, serialiseScript)
import Plutarch.Prelude
import Data.Aeson (KeyValue ((.=)), object)
import Data.Aeson.Encode.Pretty (encodePretty)
import PlutusLedgerApi.V2 (
  Data,
  ExBudget,
 )
import Data.Bifunctor (
  first,
 )

main :: IO ()
main = do
  writeTypedScript def "mintingOracleNFT" "./compiled/oracleNFTMintingPolicy.plutus" OracleNFT.oracleNFTPolicy
  writeTypedScript def "collateralValidator" "./compiled/collateralValidator.plutus" Collateral.collateralValidator
  writeTypedScript def "priceFeedOracle" "./compiled/priceFeedOracleValidator.plutus" PriceFeedOracle.priceFeedOracleValidator
  writeTypedScript def "mintingDUSD" "./compiled/dUSDMintingPolicy.plutus" Minting.mintingDUSDPolicy