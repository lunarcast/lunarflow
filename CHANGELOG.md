## [1.2.4](https://github.com/lunarcast/lunarflow/compare/v1.2.3...v1.2.4) (2020-11-01)


### Bug Fixes

* **client:** draw boxes for root lambdas ([1c74dc7](https://github.com/lunarcast/lunarflow/commit/1c74dc76b138a574a1ea82ab5dcd6e097ee64156))

## [1.2.3](https://github.com/lunarcast/lunarflow/compare/v1.2.2...v1.2.3) (2020-10-31)


### Bug Fixes

* **client:** the y alignment should be less trash now ([0e80598](https://github.com/lunarcast/lunarflow/commit/0e80598522f49e4af333c97409cbf166589d63c1))

## [1.2.2](https://github.com/lunarcast/lunarflow/compare/v1.2.1...v1.2.2) (2020-10-31)


### Bug Fixes

* **client:** removed unnecessary background for the root lambda ([9b45b8e](https://github.com/lunarcast/lunarflow/commit/9b45b8e5473000d4ad641b1d484e980d44f0d68e))
* **client:** the rendering should be centered now ([dfacec6](https://github.com/lunarcast/lunarflow/commit/dfacec6f1f30d2e22c42cb79674241fb0a2b2562))

## [1.2.1](https://github.com/lunarcast/lunarflow/compare/v1.2.0...v1.2.1) (2020-10-31)


### Bug Fixes

* **client:** added missing imports, the project should actually compile now ([5a14a4d](https://github.com/lunarcast/lunarflow/commit/5a14a4db7204db9b1798c36aa7ed2089637bbb64))
* **client:** the default expression is consistent with the initial input text ([e3f6308](https://github.com/lunarcast/lunarflow/commit/e3f6308c91c0e2adfb1ef846470aa7eeed1fadf7))

# [1.2.0](https://github.com/lunarcast/lunarflow/compare/v1.1.0...v1.2.0) (2020-10-31)


### Bug Fixes

* **client:** proper render order of call diagonals ([235b61c](https://github.com/lunarcast/lunarflow/commit/235b61c96097f0797afe659dd26e52ecf3713ca4))
* **core:** the parser now properly supports calls ([9f92059](https://github.com/lunarcast/lunarflow/commit/9f92059c750752ecbb423ba29273520c00b8ce82))
* **geometry:** fixed issues with the null shape ([e8da3b0](https://github.com/lunarcast/lunarflow/commit/e8da3b07bed3e5273d80a00f4f6838e9f3a1d219))
* **geometry:** workaround 'bounds' not taking transforms into consideration ([05e7570](https://github.com/lunarcast/lunarflow/commit/05e7570588f9c2488bc44810a587f67fe19ee18c))


### Features

* **benchmark:** benchmarking package ([d8866f8](https://github.com/lunarcast/lunarflow/commit/d8866f806900afb209dc6dc778924e3e5aa9e2a5))
* **client:** a little better looking ig? ([7d7fc1a](https://github.com/lunarcast/lunarflow/commit/7d7fc1a9193f260ffa7df1ff82ab4f86c787dd78))
* **client:** a text input which allows you to type in expressions you want to render ([07f596c](https://github.com/lunarcast/lunarflow/commit/07f596c80471352db9d06b404903d822e0148951))
* **client:** auto reziseable canvas ([892e0a3](https://github.com/lunarcast/lunarflow/commit/892e0a35ca15b7da46166dc5bd0269df9ef47a0e))
* **client:** changed the tiny existing render code to use the updated core ([8ba7f2a](https://github.com/lunarcast/lunarflow/commit/8ba7f2adde660a1e1fd20a3697c8365dfb03c10b))
* **client:** Colors + vertical spacing ([7f6a102](https://github.com/lunarcast/lunarflow/commit/7f6a10224fb384e9078bb47d541b11d265f152d0))
* **client:** compact lambdas ([322d6c1](https://github.com/lunarcast/lunarflow/commit/322d6c1c5f895703cd33a500187110445ee55958))
* **client:** diagonals ([02214f9](https://github.com/lunarcast/lunarflow/commit/02214f9c970b9b64b7662c98f2d82b32daa3c507))
* **client:** proper vertical alignment of lambda continuations ([0789ac0](https://github.com/lunarcast/lunarflow/commit/0789ac096c4fa72e0e269b9c6b1acd9dbb1d29c8))
* **client:** rendering nested lambdas should work now ([ee5792d](https://github.com/lunarcast/lunarflow/commit/ee5792de5e465895b4d61143524272f5524b4180))
* **client:** reworked height system ([97fdf61](https://github.com/lunarcast/lunarflow/commit/97fdf61effd2b126ad2825adef0efaf8a8cefd8b))
* **client:** updated the client to use the new Mu wrapper ([0aa7dde](https://github.com/lunarcast/lunarflow/commit/0aa7dde39324816a8a92e89d368ca9278f593d45))
* **client:** updated to use the new core ([ad40e75](https://github.com/lunarcast/lunarflow/commit/ad40e757bcc99ed60cb24f623738a65c30bb5d7f))
* **client:** working basic renderer ([6f677f1](https://github.com/lunarcast/lunarflow/commit/6f677f1ed9e7b8d4b597d99af5d649edbe587909))
* **core:** improved layout generation ([72068c9](https://github.com/lunarcast/lunarflow/commit/72068c9463634ac944f5b1abbde7c1d322777f79))
* **core:** more examples ([17d4df8](https://github.com/lunarcast/lunarflow/commit/17d4df816ea3a3b090d7100b8d9824a58d95db29))
* **core:** now the core uses recursion schemes ([8d06e63](https://github.com/lunarcast/lunarflow/commit/8d06e6364023043a1aa742684d51817b8fc1d6da))
* **core:** rewrote the whole layout generation algorithm ([81195df](https://github.com/lunarcast/lunarflow/commit/81195dfdf5cbe73d4da8b26a6593d723dd2f5023))
* **core:** scoped and unscoped layouts ([75e3224](https://github.com/lunarcast/lunarflow/commit/75e322420ce921562978b6c40854195f665769ef))
* **core:** updated the core to use the new Mu wrapper ([9e2dc9a](https://github.com/lunarcast/lunarflow/commit/9e2dc9a49a6e73c70fceba95c035ef16f672eeba))
* **geometry:** 2 more helpers (right bound and uniform padding) and fixed the bounds helper ([3cfd719](https://github.com/lunarcast/lunarflow/commit/3cfd719b788d51847ca7256c931322dcfd3202ae))
* **geometry:** A few helpers for working with vectors ([0070f80](https://github.com/lunarcast/lunarflow/commit/0070f805250a057c63f94c9a7f230a18749e3ae3))
* **geometry:** finding the bounds of shapes ([17bd782](https://github.com/lunarcast/lunarflow/commit/17bd7820049f5df1286d163671ddef0678bc27f3))
* **geometry:** migrated to undefined-is-not-a-problem ([45d2372](https://github.com/lunarcast/lunarflow/commit/45d2372d76dcbdc608100a7976f0e9d70d9338f7))
* **geometry:** More attribs and helpers ([8fcf2af](https://github.com/lunarcast/lunarflow/commit/8fcf2af8dac6a746333dbf7e810620427a87623f))
* **geometry:** The Shape type now uses recursion schemes ([32debc0](https://github.com/lunarcast/lunarflow/commit/32debc0e7d60616cd7e6bd991efc9d9f7c96cc5f))
* **utils:** a bunch of list and array helpers ([f250690](https://github.com/lunarcast/lunarflow/commit/f25069082cc181c03ede1e5837910632f7388982))
* **utils:** a debugSpy helper + wrapper around Mu with a Debug instance. ([a528b23](https://github.com/lunarcast/lunarflow/commit/a528b232bd2dceb25025385a72b4deb70378b279))
* **utils:** added a tiny alias of # to |> ([0d58cf9](https://github.com/lunarcast/lunarflow/commit/0d58cf98dcf5ee21aef22c6238a9b9759581c638))
* **utils:** compile time warnings when I use the Label typeclass ([b4491b4](https://github.com/lunarcast/lunarflow/commit/b4491b4bed03025888cf526f1b69041ca1801d4e))
* **utils:** moved the profiling stuff here ([3649dd4](https://github.com/lunarcast/lunarflow/commit/3649dd4a484977e0911af01add5cea33547af8fb))

# [1.1.0](https://github.com/lunarcast/lunarflow/compare/v1.0.0...v1.1.0) (2020-10-10)


### Features

* **client:** basic demo shapes on a canvas ([5cdf3d1](https://github.com/lunarcast/lunarflow/commit/5cdf3d1834f5c273b263ed471d90a76c5ee800c1))
* **geometry:** Basic geometry shapes and stuff ([3e20ed9](https://github.com/lunarcast/lunarflow/commit/3e20ed9215604e65b93f477e8da4e0f00ca63ea7))
* **utils:** created a PartialRow typeclass ([5b25080](https://github.com/lunarcast/lunarflow/commit/5b25080a480c3c37af67aa3f4c247a1f30ae5886))

# 1.0.0 (2020-10-05)


### Bug Fixes

* parsing now supports -> for lambdas ([f5b1b40](https://github.com/lunarcast/lunarflow/commit/f5b1b402ad7aa31bead76b7cf492845f457f6a07))


### Features

* a lot more work put into layout generation ([9651a97](https://github.com/lunarcast/lunarflow/commit/9651a97aeac03dcf29d6850f9adf5bb239fd0442))
* basic ast stuff ([19f3043](https://github.com/lunarcast/lunarflow/commit/19f30431831c61b2017978cdc51f1f9d9e7fb11b))
* de brujin indices ([0a540f2](https://github.com/lunarcast/lunarflow/commit/0a540f2223b356c1e9e100db422f5363815b7467))
* even more stuff for layout generation ([9353ec0](https://github.com/lunarcast/lunarflow/commit/9353ec0d779883114068ebd16032b83de88ad921))
* generating the ending points for lambda arguments ([8c45b19](https://github.com/lunarcast/lunarflow/commit/8c45b199ec57c39a2be7ac5fdde8375922f28b39))
* layout generating actually works now!!! ([53b489f](https://github.com/lunarcast/lunarflow/commit/53b489f258fbf6112e62bd0127d1d41667e5fe75))
* parser ([40c5f62](https://github.com/lunarcast/lunarflow/commit/40c5f626a48dbe01c063e0d1bf63935dd76f1cd5))
* removed the lambda grouping thing ([9cdc78c](https://github.com/lunarcast/lunarflow/commit/9cdc78c813227438b2ac43ffec518b947d400250))
* started working on layouts ([3bb8cf7](https://github.com/lunarcast/lunarflow/commit/3bb8cf7d91a78cbf039b7b572fe1a020346513f6))
* var tracking while generating layouts and the LayoutM monad ([90b36d0](https://github.com/lunarcast/lunarflow/commit/90b36d01b1ee71b2dc33831b0184e9dc7becc1ec))
