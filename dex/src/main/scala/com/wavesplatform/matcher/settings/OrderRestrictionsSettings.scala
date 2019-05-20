package com.wavesplatform.matcher.settings

import cats.data.NonEmptyList
import cats.implicits._
import com.wavesplatform.settings.utils.ConfigSettingsValidator
import com.wavesplatform.settings.utils.ConfigSettingsValidator.{ErrorsListOr, _}
import monix.eval.Coeval
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ValueReader
import play.api.libs.json.{JsObject, Json}

case class OrderRestrictionsSettings(stepAmount: Double,
                                     minAmount: Double,
                                     maxAmount: Double,
                                     stepPrice: Double,
                                     minPrice: Double,
                                     maxPrice: Double) {

  import OrderRestrictionsSettings._

  // TODO
  def getJson: Coeval[JsObject] = Coeval.evalOnce {
    Json.obj(
      "stepAmount" -> formatValue(stepAmount),
      "minAmount"  -> formatValue(minAmount),
      "maxAmount"  -> formatValue(maxAmount),
      "stepPrice"  -> formatValue(stepPrice),
      "minPrice"   -> formatValue(minPrice),
      "maxPrice"   -> formatValue(maxPrice)
    )
  }
}

object OrderRestrictionsSettings {

  val stepSizeDefault, tickSizeDefault, minAmountDefault, minPriceDefault = 0.00000001
  val maxAmountDefault                                                    = 1000000000
  val maxPriceDefault                                                     = 1000000

  def formatValue(value: Double): String = new java.text.DecimalFormat("#.########").format(value)

  implicit val orderRestrictionsSettingsReader: ValueReader[OrderRestrictionsSettings] = { (cfg, path) =>
    val cfgValidator = ConfigSettingsValidator(cfg)

    def validateSizeMinMax(sizeSettingName: String,
                           minSettingName: String,
                           maxSettingName: String,
                           sizeDefaultValue: Double,
                           minDefaultValue: Double,
                           maxDefaultValue: Double): ErrorsListOr[(Double, Double, Double)] = {

      def validateSetting(settingName: String, defaultValue: Double): ErrorsListOr[Double] =
        cfgValidator.validateByPredicateWithDefault[Double](settingName)(_ > 0, s"required 0 < value", defaultValue)

      (
        validateSetting(sizeSettingName, sizeDefaultValue),
        validateSetting(minSettingName, minDefaultValue),
        validateSetting(maxSettingName, maxDefaultValue)
      ).mapN(Tuple3.apply)
        .ensure(NonEmptyList(s"Required $minSettingName < $maxSettingName", Nil)) { case (_, min, max) => min < max }
    }

    (
      validateSizeMinMax(s"$path.step-amount", s"$path.min-amount", s"$path.max-amount", stepSizeDefault, minAmountDefault, maxAmountDefault),
      validateSizeMinMax(s"$path.step-price", s"$path.min-price", s"$path.max-price", tickSizeDefault, minPriceDefault, maxPriceDefault),
    ) mapN {
      case ((stepSize, minAmount, maxAmount), (tickSize, minPrice, maxPrice)) =>
        OrderRestrictionsSettings(stepSize, minAmount, maxAmount, tickSize, minPrice, maxPrice)
    } getValueOrThrowErrors
  }
}
