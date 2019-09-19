package com.wavesplatform.api

import com.wavesplatform.api.grpc.BlockchainUpdatesF
import io.grpc.Metadata

class BlockchainUpdatesAPIHandler[M[_]] extends BlockchainUpdatesF[M, Metadata] {

}
