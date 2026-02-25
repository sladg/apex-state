# Changelog


## [v3.9.1](https://github.com/sladg/apex-state/compare/v3.9.0...v3.9.1)

* fix: add missing pair helper re-exports to testing module [[3722e4a14fc5def7f0b843093317501923cf4d5c](https://github.com/sladg/apex-state/commit/3722e4a14fc5def7f0b843093317501923cf4d5c))]


## [v3.9.0](https://github.com/sladg/apex-state/compare/v3.7.3...v3.9.0)

* feat: add type-safe listeners() helper with lazy validation and scoped return types [[76c7053b6c035bfdcb257dc0a195db7b3aef2df0](https://github.com/sladg/apex-state/commit/76c7053b6c035bfdcb257dc0a195db7b3aef2df0))]
* perf: resolve TS2589 for pair types on large state types (>1,500 paths) [[477c473eddc2b363d4bf122c41513a9bcde95c6f](https://github.com/sladg/apex-state/commit/477c473eddc2b363d4bf122c41513a9bcde95c6f))]


## [v3.7.3](https://github.com/sladg/apex-state/compare/v3.7.1...v3.7.3)

* fix: rewrite DeepPartial as conditional type to correctly handle nullable arrays [[d7dfa13426fad0b914662c2252b03e613e9dfea4](https://github.com/sladg/apex-state/commit/d7dfa13426fad0b914662c2252b03e613e9dfea4))]
* chore: switch test environment from jsdom to happy-dom [[97398cdf1ecaca28cc4e00f86fe5f7667b1fdc87](https://github.com/sladg/apex-state/commit/97398cdf1ecaca28cc4e00f86fe5f7667b1fdc87))]


## [v3.7.1](https://github.com/sladg/apex-state/compare/v3.7.0...v3.7.1)

* fix: replace structuredClone with deepClone in applyChangesToObject [[424d4a9d69f49040faf31a83af93f03acbd01bbf](https://github.com/sladg/apex-state/commit/424d4a9d69f49040faf31a83af93f03acbd01bbf))]


## [v3.7.0](https://github.com/sladg/apex-state/compare/v3.6.3...v3.7.0)

* feat: add CONTAINS_ANY and CONTAINS_ALL to BoolLogic; improve is utility [[22a5cdd07435725bae185735c8e8b071f96b96f9](https://github.com/sladg/apex-state/commit/22a5cdd07435725bae185735c8e8b071f96b96f9))]


## [v3.6.3](https://github.com/sladg/apex-state/compare/v3.6.2...v3.6.3)

* fix: DeepValue now strips null/undefined from intermediate types during path traversal [[fe866b359385fe4df9380273cdad504c30c7a5d2](https://github.com/sladg/apex-state/commit/fe866b359385fe4df9380273cdad504c30c7a5d2))]


## [v3.6.2](https://github.com/sladg/apex-state/compare/v3.6.1...v3.6.2)

* fix: fix relative links in llms.txt and add llms-full.txt reference [[f4f49768671f515764d05ecbfbd5b1230ff9f967](https://github.com/sladg/apex-state/commit/f4f49768671f515764d05ecbfbd5b1230ff9f967))]


## [v3.6.1](https://github.com/sladg/apex-state/compare/v3.6.0...v3.6.1)

* docs: document BoolLogic shorthand and fix stale file paths [[4329b44ea5ce6e7b3639099586021000c674d478](https://github.com/sladg/apex-state/commit/4329b44ea5ce6e7b3639099586021000c674d478))]


## [v3.6.0](https://github.com/sladg/apex-state/compare/v3.5.1...v3.6.0)

* feat: add shorthand [path, value] syntax to BoolLogic with full test coverage [[d07d33a361d8531d1ee158fd14cd53f5ec13be27](https://github.com/sladg/apex-state/commit/d07d33a361d8531d1ee158fd14cd53f5ec13be27))]


## [v3.5.1](https://github.com/sladg/apex-state/compare/v3.5.0...v3.5.1)

* chore: deduplicate benchmark fixtures and remove dead test code [[08cc7a073b0d10ea3bea3da09c391a60fce29099](https://github.com/sladg/apex-state/commit/08cc7a073b0d10ea3bea3da09c391a60fce29099))]


## [v3.5.0](https://github.com/sladg/apex-state/compare/v3.4.2...v3.5.0)

* feat: add configurable depth to DeepKey with ?? marker for any-typed properties [[2e38df5684ab0a5f3eebc4516968757467bafc0c](https://github.com/sladg/apex-state/commit/2e38df5684ab0a5f3eebc4516968757467bafc0c))]


## [v3.4.2](https://github.com/sladg/apex-state/compare/v3.4.1...v3.4.2)

* fix: disable code splitting to prevent top-level await in published output [[d08aeeab21a34e9b23349541062f4968be1ef417](https://github.com/sladg/apex-state/commit/d08aeeab21a34e9b23349541062f4968be1ef417))]


## [v3.4.1](https://github.com/sladg/apex-state/compare/v3.4.0...v3.4.1)

* chore: clean up docs, add examples, remove legacy tests and migration comments [[b7447dbde9a64425a0d4a09d8f41fbd091320573](https://github.com/sladg/apex-state/commit/b7447dbde9a64425a0d4a09d8f41fbd091320573))]


## [v3.4.0](https://github.com/sladg/apex-state/compare/v3.3.0...v3.4.0)

* feat: add interactive demo with GH Pages deployment [[4de0ee870dc0dec9037b2229ae7cc8f1ae4ba109](https://github.com/sladg/apex-state/commit/4de0ee870dc0dec9037b2229ae7cc8f1ae4ba109))]


## [v3.3.0](https://github.com/sladg/apex-state/compare/v3.2.1...v3.3.0)

* feat: add excludeWhen aggregation, computation SUM/AVG, optimize Rust hot paths, improve WASM bridge types [[f11f4a33ef307ff8fec0334b499bd49d3afd1f16](https://github.com/sladg/apex-state/commit/f11f4a33ef307ff8fec0334b499bd49d3afd1f16))]


## [v3.2.1](https://github.com/sladg/apex-state/compare/v3.2.0...v3.2.1)

* fix: simplify Provider with WasmGate + useRef init, fix StrictMode pipeline null [[c90c321bd5d1545f67ffcd4d2fde6501e07c5c49](https://github.com/sladg/apex-state/commit/c90c321bd5d1545f67ffcd4d2fde6501e07c5c49))]


## [v3.2.0](https://github.com/sladg/apex-state/compare/v3.1.0...v3.2.0)

* feat: add @sladg/apex-state/testing subpath with mock store and __mocked control API [[031556fb641834651429e0ea47cd1487a3008893](https://github.com/sladg/apex-state/commit/031556fb641834651429e0ea47cd1487a3008893))]


## [v3.1.0](https://github.com/sladg/apex-state/compare/v3.0.5...v3.1.0)

* fix(wasm): treat undefined sentinel as null-like in shadow state traversal [[049d0101a9a41347fb37b159f9f33ce5dfd63354](https://github.com/sladg/apex-state/commit/049d0101a9a41347fb37b159f9f33ce5dfd63354))]
* feat: replace Zod peer dependency with ValidationSchema interface and support React 19 [[230bd9b28d31e29287aba7afc5852d7860a169cd](https://github.com/sladg/apex-state/commit/230bd9b28d31e29287aba7afc5852d7860a169cd))]


## [v3.0.5](https://github.com/sladg/apex-state/compare/v3.0.4...v3.0.5)

* fix(provider): refactor WASM readiness to single-effect lifecycle [[b408c2ccbc53456ce3bf5d993617f9d77c2a3f2a](https://github.com/sladg/apex-state/commit/b408c2ccbc53456ce3bf5d993617f9d77c2a3f2a))]


## [v3.0.4](https://github.com/sladg/apex-state/compare/v3.0.3...v3.0.4)

* fix(provider): restore WASM pipeline after React StrictMode cleanup [[fbd3c4eb54382729a29624a3af178c04839ae24d](https://github.com/sladg/apex-state/commit/fbd3c4eb54382729a29624a3af178c04839ae24d))]


## [v3.0.3](https://github.com/sladg/apex-state/compare/v3.0.2...v3.0.3)

* fix: aggregation undefined sentinel and concern type safety [[74cb2b05a422c5daafe8574a0cff62aa587e1e83](https://github.com/sladg/apex-state/commit/74cb2b05a422c5daafe8574a0cff62aa587e1e83))]


## [v3.0.2](https://github.com/sladg/apex-state/compare/v3.0.1...v3.0.2)

* fix(build): set platform to browser for browser-compatible WASM embedding [[f6c6c222b59500444e0d234954669463654645d4](https://github.com/sladg/apex-state/commit/f6c6c222b59500444e0d234954669463654645d4))]


## [v3.0.1](https://github.com/sladg/apex-state/compare/v3.0.0...v3.0.1)

* fix: set npm publish access to public for scoped package [[c04a01210ad138513a334e441e9943a8685d0d04](https://github.com/sladg/apex-state/commit/c04a01210ad138513a334e441e9943a8685d0d04))]


## [v3.0.0](https://github.com/sladg/apex-state/compare/v0.0.1...v3.0.0)

* fix: rename .wasm.ts files to .wasm-impl.ts to avoid esbuild wasm plugin conflict [[d3b9a36ddcb8bb884df24f252abf3cc9ab3d7567](https://github.com/sladg/apex-state/commit/d3b9a36ddcb8bb884df24f252abf3cc9ab3d7567))]
* feat: JSON serialization optimization, CI publishing, aggregation refactor [[85edf22c00b470f61df5f0ea58f214d22b3de9e2](https://github.com/sladg/apex-state/commit/85edf22c00b470f61df5f0ea58f214d22b3de9e2))]
* refactor(rust): audit #[allow(dead_code)] annotations, fix clippy warnings [[7151ca6bf89ccadd2c64df7331e8b5be50692123](https://github.com/sladg/apex-state/commit/7151ca6bf89ccadd2c64df7331e8b5be50692123))]
* refactor: WASM multi-store isolation, clear-paths implementation, v2 test suite [[243ef1e3694eaea6655bfc132641a6bd4f2c0402](https://github.com/sladg/apex-state/commit/243ef1e3694eaea6655bfc132641a6bd4f2c0402))]
* feat(value-logic): new engine extension added for simple statically defined if/else logic [[fbc08862c4107b9f6a0f03b2159839f9741e4ebc](https://github.com/sladg/apex-state/commit/fbc08862c4107b9f6a0f03b2159839f9741e4ebc))]
* feat(wasm): updated new version, docs aded, benchmarks. pipelines and new tests for verification, removed obsolete and wrong bits and pieces [[b8cf6fdf1f110944a4b3e6a0d042af8b37e2c22d](https://github.com/sladg/apex-state/commit/b8cf6fdf1f110944a4b3e6a0d042af8b37e2c22d))]
* refactor: rename files to kebab-case, implement v2 integration tests, update WASM pipeline [[d4cb5459b37e6242c3eea6740ce95f5e6e3325e5](https://github.com/sladg/apex-state/commit/d4cb5459b37e6242c3eea6740ce95f5e6e3325e5))]
* feat(wasm): fixed missing bits in wasm, updated test scaffolding, debugging capability [[5d56e6ca09e186b8edb4395bc27e1db411ff49f3](https://github.com/sladg/apex-state/commit/5d56e6ca09e186b8edb4395bc27e1db411ff49f3))]
* feat(wasm): improved code, warnings eliminated, updated docs and removed phase naming [[2fa5ef883dbbe35a292ff77a3b820850fa7506dc](https://github.com/sladg/apex-state/commit/2fa5ef883dbbe35a292ff77a3b820850fa7506dc))]
* feat(wasm): pipeline update, simplified typescript pipeline handler [[22199cfe61d5582268d08a5e216f6e5142f2d671](https://github.com/sladg/apex-state/commit/22199cfe61d5582268d08a5e216f6e5142f2d671))]
* feat(wasm): optimize writes and filter out no-op changes [[b9b89b1a132041e974e6999c523d72079ce188bc](https://github.com/sladg/apex-state/commit/b9b89b1a132041e974e6999c523d72079ce188bc))]
* feat(wasm): diff engine [[c0bf901b415fedb10d616894cc99748946f3d7ce](https://github.com/sladg/apex-state/commit/c0bf901b415fedb10d616894cc99748946f3d7ce))]
* feat(wasm): validations [[772aa47f9687cc8fb5547db4c01061e3e9a91248](https://github.com/sladg/apex-state/commit/772aa47f9687cc8fb5547db4c01061e3e9a91248))]
* feat(wasm): allow for validations (zod) in rust [[94f077024c36a4456cfec66d34b7a72ab11f4bf5](https://github.com/sladg/apex-state/commit/94f077024c36a4456cfec66d34b7a72ab11f4bf5))]
* feat(wasm): optimizations, test placeholders, speed comparisons [[fa385db51219dfada58ac95258484bb788fb7717](https://github.com/sladg/apex-state/commit/fa385db51219dfada58ac95258484bb788fb7717))]
* feat(wasm): pipelines, graphs, embeddeding of wasm inline [[f7afce21081527247b4538b8597469ae6c8be44a](https://github.com/sladg/apex-state/commit/f7afce21081527247b4538b8597469ae6c8be44a))]
* feat(wasm): initial rebuild, base-line rust code in place, basic bridge added [[705dc4fbcd3066bd66648487d5eac552f21a6fa6](https://github.com/sladg/apex-state/commit/705dc4fbcd3066bd66648487d5eac552f21a6fa6))]
* docs(wasm): updated claude.md, added wasm architecture and task for processing [[a55c66448d17eb47772fb7458ee5d520772d6a6e](https://github.com/sladg/apex-state/commit/a55c66448d17eb47772fb7458ee5d520772d6a6e))]
* Implement Shadow State as Nested Tree in Rust/WASM (#13) [[22ee86b332b74c99a3f33ff45193ce46ea9095b4](https://github.com/sladg/apex-state/commit/22ee86b332b74c99a3f33ff45193ce46ea9095b4))]
* WASM-004: BoolLogic evaluator with tuple variants (#12) [[fcfe40e28b4fc28f1dccbbfb3bf5d3f1a9c2d7f6](https://github.com/sladg/apex-state/commit/fcfe40e28b4fc28f1dccbbfb3bf5d3f1a9c2d7f6))]
* Shadow State as Nested Object Tree (#11) [[ad6c1c42607b427318924d828a5576dd427daf50](https://github.com/sladg/apex-state/commit/ad6c1c42607b427318924d828a5576dd427daf50))]
* WASM Internal String Interning for Path Handling (#10) [[f730ff537dde92435bc11cebed7ad2645933e274](https://github.com/sladg/apex-state/commit/f730ff537dde92435bc11cebed7ad2645933e274))]
* Rust Toolchain for WASM Compilation (#9) [[c10c6b06e6b1dbff9d2267533b4fa66b7c9867e6](https://github.com/sladg/apex-state/commit/c10c6b06e6b1dbff9d2267533b4fa66b7c9867e6))]
* feat(core): add debug timing system, batch sync registration, and config restructure [[d221b604121dab803e1eff869ca0e8639e15685c](https://github.com/sladg/apex-state/commit/d221b604121dab803e1eff869ca0e8639e15685c))]
* docs: reorganize documentation structure and add expert agent guides [[c2c957e0835d0e836058923bf389444cd65b111e](https://github.com/sladg/apex-state/commit/c2c957e0835d0e836058923bf389444cd65b111e))]
* feat(hooks): add useThrottledField hook for rate-limited setValue calls [[1399fb6abc7e44a961d313e3797feff6dec20ba9](https://github.com/sladg/apex-state/commit/1399fb6abc7e44a961d313e3797feff6dec20ba9))]
* perf(core): replace graphology with PathGroups for O(1) connected components [[aee814016a8c174574856165bfd1cd3a68bfe409](https://github.com/sladg/apex-state/commit/aee814016a8c174574856165bfd1cd3a68bfe409))]
* refactor(hooks): replace monolithic transform hook with composable hooks [[f11cecaf2ed5e4e59cc455ce0536781afde5d24e](https://github.com/sladg/apex-state/commit/f11cecaf2ed5e4e59cc455ce0536781afde5d24e))]
* ref(core): cleanup of store, de-duplicated hook functions, fixed return type of validation concern [[40e789acd98c8aa787bcd8177a2707b2eeb8af3f](https://github.com/sladg/apex-state/commit/40e789acd98c8aa787bcd8177a2707b2eeb8af3f))]
* feat(hashmaps): allow for dynamic hashmaps to be used in objects, correctly infer types [[15bee6f13a4de81732c5169a3985dc77202aaa12](https://github.com/sladg/apex-state/commit/15bee6f13a4de81732c5169a3985dc77202aaa12))]
* refactor(core): replace deepAccess with dot util; simplify accessors and update pipelines [[7359c8d6cd546e0c838f46534d224c9c853f33b2](https://github.com/sladg/apex-state/commit/7359c8d6cd546e0c838f46534d224c9c853f33b2))]
* perf: replace lodash get/set with native Reflect — 2-3.6x faster (#3) [[70d0716e880df6281a728692cf0b58704f146673](https://github.com/sladg/apex-state/commit/70d0716e880df6281a728692cf0b58704f146673))]
* tests(normalize changes): added tests to ensure correct behaviour [[d27227412088c3a1920f6328d95fb12ba8e066c4](https://github.com/sladg/apex-state/commit/d27227412088c3a1920f6328d95fb12ba8e066c4))]
* docs(dead-code): removed [[5e5bc165f438e4c71488dd600d1874e9fa292632](https://github.com/sladg/apex-state/commit/5e5bc165f438e4c71488dd600d1874e9fa292632))]
* tests(reuse): deduplicated and reused shared setups in tests [[8f1412eee5f4d93266f241ff838fe413c27bc03d](https://github.com/sladg/apex-state/commit/8f1412eee5f4d93266f241ff838fe413c27bc03d))]
* refactor(mocks): consolidate stores into index.ts, remove stores.ts [[d69c84500bb73d873115fe0dc56f4379a454b63c](https://github.com/sladg/apex-state/commit/d69c84500bb73d873115fe0dc56f4379a454b63c))]
* fix(tests): resolve remaining TypeScript errors with overloads and optional properties [[71827cec160ca7e81dea7c052ac0352cb21c9d7c](https://github.com/sladg/apex-state/commit/71827cec160ca7e81dea7c052ac0352cb21c9d7c))]
* fix(tests): remove unused variables, fix bracket notation, clean up test assertions [[2d06fb238615793fd07816ce60aafac6805f029c](https://github.com/sladg/apex-state/commit/2d06fb238615793fd07816ce60aafac6805f029c))]
* ref(redo): performance, aggregation handling, deduped validations, concerns improvements [[d667b86e763792102328039518fd588d2a227934](https://github.com/sladg/apex-state/commit/d667b86e763792102328039518fd588d2a227934))]
* refactor(integration tests): consolidate test utilities in group 2 [[0f7ee8898b4ef42d43878d8dd2264fdd7b0bc354](https://github.com/sladg/apex-state/commit/0f7ee8898b4ef42d43878d8dd2264fdd7b0bc354))]
* docs(readme, authors): updated [[0f6e31c162ce1b61d40f75ee6b23f3f153631353](https://github.com/sladg/apex-state/commit/0f6e31c162ce1b61d40f75ee6b23f3f153631353))]
* fix(tsc): removed used of any and other overrides, style(eslint): implement code formatting [[2b04b6cdfb8d629ee5aae369c81b404599f12db7](https://github.com/sladg/apex-state/commit/2b04b6cdfb8d629ee5aae369c81b404599f12db7))]
* feat(poc): working example, concerns, side-effects, majority of architecture in place [[37db2cf7606e5abbfeb42f81a092c5c245098c62](https://github.com/sladg/apex-state/commit/37db2cf7606e5abbfeb42f81a092c5c245098c62))]
* Fix remaining TypeScript errors with documented suppressions [[9ff505a747a3310567cbcb7d598a7a77a17f6c84](https://github.com/sladg/apex-state/commit/9ff505a747a3310567cbcb7d598a7a77a17f6c84))]
* Fix TypeScript errors in validators and test files [[5539fc3f8bc72b7bd78422c89675929673269e52](https://github.com/sladg/apex-state/commit/5539fc3f8bc72b7bd78422c89675929673269e52))]
* Enforce functional programming: Convert all to arrow functions, eliminate classes [[eb05e60e946dd52eb67a7caee3175d901fb3bf54](https://github.com/sladg/apex-state/commit/eb05e60e946dd52eb67a7caee3175d901fb3bf54))]
* Complete: Enhanced Derived Values Testing - Reactivity Verification [[c7066950b3b4d83175d46035e9fa219d9f49f868](https://github.com/sladg/apex-state/commit/c7066950b3b4d83175d46035e9fa219d9f49f868))]
* Complete Phase 8: Comprehensive README documentation [[7656a9804a842e538f0463e80c17ba6d6f70b0b1](https://github.com/sladg/apex-state/commit/7656a9804a842e538f0463e80c17ba6d6f70b0b1))]
* Implement Phase 7: Integration Testing (simplified) [[91176a2b953015cde9ac2738101ee04565a64c63](https://github.com/sladg/apex-state/commit/91176a2b953015cde9ac2738101ee04565a64c63))]
* Implement Phase 6: Advanced Type Utilities and Form Hooks (APEX-5, 8, 17, 18) [[ced5c07769b25b89a04dac0aa3c28ad6f1f3fdbf](https://github.com/sladg/apex-state/commit/ced5c07769b25b89a04dac0aa3c28ad6f1f3fdbf))]
* Remove deprecated synchronizer placeholder functions [[ae61dbdae87a4067c30fee36c82795faeaa423fc](https://github.com/sladg/apex-state/commit/ae61dbdae87a4067c30fee36c82795faeaa423fc))]


## [v0.0.1](https://github.com/sladg/apex-state/compare/v0.0.1)

* fix: add missing pair helper re-exports to testing module [[3722e4a14fc5def7f0b843093317501923cf4d5c](https://github.com/sladg/apex-state/commit/3722e4a14fc5def7f0b843093317501923cf4d5c))]
* feat: add type-safe listeners() helper with lazy validation and scoped return types [[76c7053b6c035bfdcb257dc0a195db7b3aef2df0](https://github.com/sladg/apex-state/commit/76c7053b6c035bfdcb257dc0a195db7b3aef2df0))]
* perf: resolve TS2589 for pair types on large state types (>1,500 paths) [[477c473eddc2b363d4bf122c41513a9bcde95c6f](https://github.com/sladg/apex-state/commit/477c473eddc2b363d4bf122c41513a9bcde95c6f))]
* fix: rewrite DeepPartial as conditional type to correctly handle nullable arrays [[d7dfa13426fad0b914662c2252b03e613e9dfea4](https://github.com/sladg/apex-state/commit/d7dfa13426fad0b914662c2252b03e613e9dfea4))]
* chore: switch test environment from jsdom to happy-dom [[97398cdf1ecaca28cc4e00f86fe5f7667b1fdc87](https://github.com/sladg/apex-state/commit/97398cdf1ecaca28cc4e00f86fe5f7667b1fdc87))]
* fix: replace structuredClone with deepClone in applyChangesToObject [[424d4a9d69f49040faf31a83af93f03acbd01bbf](https://github.com/sladg/apex-state/commit/424d4a9d69f49040faf31a83af93f03acbd01bbf))]
* feat: add CONTAINS_ANY and CONTAINS_ALL to BoolLogic; improve is utility [[22a5cdd07435725bae185735c8e8b071f96b96f9](https://github.com/sladg/apex-state/commit/22a5cdd07435725bae185735c8e8b071f96b96f9))]
* fix: DeepValue now strips null/undefined from intermediate types during path traversal [[fe866b359385fe4df9380273cdad504c30c7a5d2](https://github.com/sladg/apex-state/commit/fe866b359385fe4df9380273cdad504c30c7a5d2))]
* fix: fix relative links in llms.txt and add llms-full.txt reference [[f4f49768671f515764d05ecbfbd5b1230ff9f967](https://github.com/sladg/apex-state/commit/f4f49768671f515764d05ecbfbd5b1230ff9f967))]
* docs: document BoolLogic shorthand and fix stale file paths [[4329b44ea5ce6e7b3639099586021000c674d478](https://github.com/sladg/apex-state/commit/4329b44ea5ce6e7b3639099586021000c674d478))]
* feat: add shorthand [path, value] syntax to BoolLogic with full test coverage [[d07d33a361d8531d1ee158fd14cd53f5ec13be27](https://github.com/sladg/apex-state/commit/d07d33a361d8531d1ee158fd14cd53f5ec13be27))]
* chore: deduplicate benchmark fixtures and remove dead test code [[08cc7a073b0d10ea3bea3da09c391a60fce29099](https://github.com/sladg/apex-state/commit/08cc7a073b0d10ea3bea3da09c391a60fce29099))]
* feat: add configurable depth to DeepKey with ?? marker for any-typed properties [[2e38df5684ab0a5f3eebc4516968757467bafc0c](https://github.com/sladg/apex-state/commit/2e38df5684ab0a5f3eebc4516968757467bafc0c))]
* fix: disable code splitting to prevent top-level await in published output [[d08aeeab21a34e9b23349541062f4968be1ef417](https://github.com/sladg/apex-state/commit/d08aeeab21a34e9b23349541062f4968be1ef417))]
* chore: clean up docs, add examples, remove legacy tests and migration comments [[b7447dbde9a64425a0d4a09d8f41fbd091320573](https://github.com/sladg/apex-state/commit/b7447dbde9a64425a0d4a09d8f41fbd091320573))]
* feat: add interactive demo with GH Pages deployment [[4de0ee870dc0dec9037b2229ae7cc8f1ae4ba109](https://github.com/sladg/apex-state/commit/4de0ee870dc0dec9037b2229ae7cc8f1ae4ba109))]
* feat: add excludeWhen aggregation, computation SUM/AVG, optimize Rust hot paths, improve WASM bridge types [[f11f4a33ef307ff8fec0334b499bd49d3afd1f16](https://github.com/sladg/apex-state/commit/f11f4a33ef307ff8fec0334b499bd49d3afd1f16))]
* fix: simplify Provider with WasmGate + useRef init, fix StrictMode pipeline null [[c90c321bd5d1545f67ffcd4d2fde6501e07c5c49](https://github.com/sladg/apex-state/commit/c90c321bd5d1545f67ffcd4d2fde6501e07c5c49))]
* feat: add @sladg/apex-state/testing subpath with mock store and __mocked control API [[031556fb641834651429e0ea47cd1487a3008893](https://github.com/sladg/apex-state/commit/031556fb641834651429e0ea47cd1487a3008893))]
* fix(wasm): treat undefined sentinel as null-like in shadow state traversal [[049d0101a9a41347fb37b159f9f33ce5dfd63354](https://github.com/sladg/apex-state/commit/049d0101a9a41347fb37b159f9f33ce5dfd63354))]
* feat: replace Zod peer dependency with ValidationSchema interface and support React 19 [[230bd9b28d31e29287aba7afc5852d7860a169cd](https://github.com/sladg/apex-state/commit/230bd9b28d31e29287aba7afc5852d7860a169cd))]
* fix(provider): refactor WASM readiness to single-effect lifecycle [[b408c2ccbc53456ce3bf5d993617f9d77c2a3f2a](https://github.com/sladg/apex-state/commit/b408c2ccbc53456ce3bf5d993617f9d77c2a3f2a))]
* fix(provider): restore WASM pipeline after React StrictMode cleanup [[fbd3c4eb54382729a29624a3af178c04839ae24d](https://github.com/sladg/apex-state/commit/fbd3c4eb54382729a29624a3af178c04839ae24d))]
* fix: aggregation undefined sentinel and concern type safety [[74cb2b05a422c5daafe8574a0cff62aa587e1e83](https://github.com/sladg/apex-state/commit/74cb2b05a422c5daafe8574a0cff62aa587e1e83))]
* fix(build): set platform to browser for browser-compatible WASM embedding [[f6c6c222b59500444e0d234954669463654645d4](https://github.com/sladg/apex-state/commit/f6c6c222b59500444e0d234954669463654645d4))]
* fix: set npm publish access to public for scoped package [[c04a01210ad138513a334e441e9943a8685d0d04](https://github.com/sladg/apex-state/commit/c04a01210ad138513a334e441e9943a8685d0d04))]
* fix: rename .wasm.ts files to .wasm-impl.ts to avoid esbuild wasm plugin conflict [[d3b9a36ddcb8bb884df24f252abf3cc9ab3d7567](https://github.com/sladg/apex-state/commit/d3b9a36ddcb8bb884df24f252abf3cc9ab3d7567))]
* feat: JSON serialization optimization, CI publishing, aggregation refactor [[85edf22c00b470f61df5f0ea58f214d22b3de9e2](https://github.com/sladg/apex-state/commit/85edf22c00b470f61df5f0ea58f214d22b3de9e2))]
* refactor(rust): audit #[allow(dead_code)] annotations, fix clippy warnings [[7151ca6bf89ccadd2c64df7331e8b5be50692123](https://github.com/sladg/apex-state/commit/7151ca6bf89ccadd2c64df7331e8b5be50692123))]
* refactor: WASM multi-store isolation, clear-paths implementation, v2 test suite [[243ef1e3694eaea6655bfc132641a6bd4f2c0402](https://github.com/sladg/apex-state/commit/243ef1e3694eaea6655bfc132641a6bd4f2c0402))]
* feat(value-logic): new engine extension added for simple statically defined if/else logic [[fbc08862c4107b9f6a0f03b2159839f9741e4ebc](https://github.com/sladg/apex-state/commit/fbc08862c4107b9f6a0f03b2159839f9741e4ebc))]
* feat(wasm): updated new version, docs aded, benchmarks. pipelines and new tests for verification, removed obsolete and wrong bits and pieces [[b8cf6fdf1f110944a4b3e6a0d042af8b37e2c22d](https://github.com/sladg/apex-state/commit/b8cf6fdf1f110944a4b3e6a0d042af8b37e2c22d))]
* refactor: rename files to kebab-case, implement v2 integration tests, update WASM pipeline [[d4cb5459b37e6242c3eea6740ce95f5e6e3325e5](https://github.com/sladg/apex-state/commit/d4cb5459b37e6242c3eea6740ce95f5e6e3325e5))]
* feat(wasm): fixed missing bits in wasm, updated test scaffolding, debugging capability [[5d56e6ca09e186b8edb4395bc27e1db411ff49f3](https://github.com/sladg/apex-state/commit/5d56e6ca09e186b8edb4395bc27e1db411ff49f3))]
* feat(wasm): improved code, warnings eliminated, updated docs and removed phase naming [[2fa5ef883dbbe35a292ff77a3b820850fa7506dc](https://github.com/sladg/apex-state/commit/2fa5ef883dbbe35a292ff77a3b820850fa7506dc))]
* feat(wasm): pipeline update, simplified typescript pipeline handler [[22199cfe61d5582268d08a5e216f6e5142f2d671](https://github.com/sladg/apex-state/commit/22199cfe61d5582268d08a5e216f6e5142f2d671))]
* feat(wasm): optimize writes and filter out no-op changes [[b9b89b1a132041e974e6999c523d72079ce188bc](https://github.com/sladg/apex-state/commit/b9b89b1a132041e974e6999c523d72079ce188bc))]
* feat(wasm): diff engine [[c0bf901b415fedb10d616894cc99748946f3d7ce](https://github.com/sladg/apex-state/commit/c0bf901b415fedb10d616894cc99748946f3d7ce))]
* feat(wasm): validations [[772aa47f9687cc8fb5547db4c01061e3e9a91248](https://github.com/sladg/apex-state/commit/772aa47f9687cc8fb5547db4c01061e3e9a91248))]
* feat(wasm): allow for validations (zod) in rust [[94f077024c36a4456cfec66d34b7a72ab11f4bf5](https://github.com/sladg/apex-state/commit/94f077024c36a4456cfec66d34b7a72ab11f4bf5))]
* feat(wasm): optimizations, test placeholders, speed comparisons [[fa385db51219dfada58ac95258484bb788fb7717](https://github.com/sladg/apex-state/commit/fa385db51219dfada58ac95258484bb788fb7717))]
* feat(wasm): pipelines, graphs, embeddeding of wasm inline [[f7afce21081527247b4538b8597469ae6c8be44a](https://github.com/sladg/apex-state/commit/f7afce21081527247b4538b8597469ae6c8be44a))]
* feat(wasm): initial rebuild, base-line rust code in place, basic bridge added [[705dc4fbcd3066bd66648487d5eac552f21a6fa6](https://github.com/sladg/apex-state/commit/705dc4fbcd3066bd66648487d5eac552f21a6fa6))]
* docs(wasm): updated claude.md, added wasm architecture and task for processing [[a55c66448d17eb47772fb7458ee5d520772d6a6e](https://github.com/sladg/apex-state/commit/a55c66448d17eb47772fb7458ee5d520772d6a6e))]
* Implement Shadow State as Nested Tree in Rust/WASM (#13) [[22ee86b332b74c99a3f33ff45193ce46ea9095b4](https://github.com/sladg/apex-state/commit/22ee86b332b74c99a3f33ff45193ce46ea9095b4))]
* WASM-004: BoolLogic evaluator with tuple variants (#12) [[fcfe40e28b4fc28f1dccbbfb3bf5d3f1a9c2d7f6](https://github.com/sladg/apex-state/commit/fcfe40e28b4fc28f1dccbbfb3bf5d3f1a9c2d7f6))]
* Shadow State as Nested Object Tree (#11) [[ad6c1c42607b427318924d828a5576dd427daf50](https://github.com/sladg/apex-state/commit/ad6c1c42607b427318924d828a5576dd427daf50))]
* WASM Internal String Interning for Path Handling (#10) [[f730ff537dde92435bc11cebed7ad2645933e274](https://github.com/sladg/apex-state/commit/f730ff537dde92435bc11cebed7ad2645933e274))]
* Rust Toolchain for WASM Compilation (#9) [[c10c6b06e6b1dbff9d2267533b4fa66b7c9867e6](https://github.com/sladg/apex-state/commit/c10c6b06e6b1dbff9d2267533b4fa66b7c9867e6))]
* feat(core): add debug timing system, batch sync registration, and config restructure [[d221b604121dab803e1eff869ca0e8639e15685c](https://github.com/sladg/apex-state/commit/d221b604121dab803e1eff869ca0e8639e15685c))]
* docs: reorganize documentation structure and add expert agent guides [[c2c957e0835d0e836058923bf389444cd65b111e](https://github.com/sladg/apex-state/commit/c2c957e0835d0e836058923bf389444cd65b111e))]
* feat(hooks): add useThrottledField hook for rate-limited setValue calls [[1399fb6abc7e44a961d313e3797feff6dec20ba9](https://github.com/sladg/apex-state/commit/1399fb6abc7e44a961d313e3797feff6dec20ba9))]
* perf(core): replace graphology with PathGroups for O(1) connected components [[aee814016a8c174574856165bfd1cd3a68bfe409](https://github.com/sladg/apex-state/commit/aee814016a8c174574856165bfd1cd3a68bfe409))]
* refactor(hooks): replace monolithic transform hook with composable hooks [[f11cecaf2ed5e4e59cc455ce0536781afde5d24e](https://github.com/sladg/apex-state/commit/f11cecaf2ed5e4e59cc455ce0536781afde5d24e))]
* ref(core): cleanup of store, de-duplicated hook functions, fixed return type of validation concern [[40e789acd98c8aa787bcd8177a2707b2eeb8af3f](https://github.com/sladg/apex-state/commit/40e789acd98c8aa787bcd8177a2707b2eeb8af3f))]
* feat(hashmaps): allow for dynamic hashmaps to be used in objects, correctly infer types [[15bee6f13a4de81732c5169a3985dc77202aaa12](https://github.com/sladg/apex-state/commit/15bee6f13a4de81732c5169a3985dc77202aaa12))]
* refactor(core): replace deepAccess with dot util; simplify accessors and update pipelines [[7359c8d6cd546e0c838f46534d224c9c853f33b2](https://github.com/sladg/apex-state/commit/7359c8d6cd546e0c838f46534d224c9c853f33b2))]
* perf: replace lodash get/set with native Reflect — 2-3.6x faster (#3) [[70d0716e880df6281a728692cf0b58704f146673](https://github.com/sladg/apex-state/commit/70d0716e880df6281a728692cf0b58704f146673))]
* tests(normalize changes): added tests to ensure correct behaviour [[d27227412088c3a1920f6328d95fb12ba8e066c4](https://github.com/sladg/apex-state/commit/d27227412088c3a1920f6328d95fb12ba8e066c4))]
* docs(dead-code): removed [[5e5bc165f438e4c71488dd600d1874e9fa292632](https://github.com/sladg/apex-state/commit/5e5bc165f438e4c71488dd600d1874e9fa292632))]
* tests(reuse): deduplicated and reused shared setups in tests [[8f1412eee5f4d93266f241ff838fe413c27bc03d](https://github.com/sladg/apex-state/commit/8f1412eee5f4d93266f241ff838fe413c27bc03d))]
* refactor(mocks): consolidate stores into index.ts, remove stores.ts [[d69c84500bb73d873115fe0dc56f4379a454b63c](https://github.com/sladg/apex-state/commit/d69c84500bb73d873115fe0dc56f4379a454b63c))]
* fix(tests): resolve remaining TypeScript errors with overloads and optional properties [[71827cec160ca7e81dea7c052ac0352cb21c9d7c](https://github.com/sladg/apex-state/commit/71827cec160ca7e81dea7c052ac0352cb21c9d7c))]
* fix(tests): remove unused variables, fix bracket notation, clean up test assertions [[2d06fb238615793fd07816ce60aafac6805f029c](https://github.com/sladg/apex-state/commit/2d06fb238615793fd07816ce60aafac6805f029c))]
* ref(redo): performance, aggregation handling, deduped validations, concerns improvements [[d667b86e763792102328039518fd588d2a227934](https://github.com/sladg/apex-state/commit/d667b86e763792102328039518fd588d2a227934))]
* refactor(integration tests): consolidate test utilities in group 2 [[0f7ee8898b4ef42d43878d8dd2264fdd7b0bc354](https://github.com/sladg/apex-state/commit/0f7ee8898b4ef42d43878d8dd2264fdd7b0bc354))]
* docs(readme, authors): updated [[0f6e31c162ce1b61d40f75ee6b23f3f153631353](https://github.com/sladg/apex-state/commit/0f6e31c162ce1b61d40f75ee6b23f3f153631353))]
* fix(tsc): removed used of any and other overrides, style(eslint): implement code formatting [[2b04b6cdfb8d629ee5aae369c81b404599f12db7](https://github.com/sladg/apex-state/commit/2b04b6cdfb8d629ee5aae369c81b404599f12db7))]
* feat(poc): working example, concerns, side-effects, majority of architecture in place [[37db2cf7606e5abbfeb42f81a092c5c245098c62](https://github.com/sladg/apex-state/commit/37db2cf7606e5abbfeb42f81a092c5c245098c62))]
* Fix remaining TypeScript errors with documented suppressions [[9ff505a747a3310567cbcb7d598a7a77a17f6c84](https://github.com/sladg/apex-state/commit/9ff505a747a3310567cbcb7d598a7a77a17f6c84))]
* Fix TypeScript errors in validators and test files [[5539fc3f8bc72b7bd78422c89675929673269e52](https://github.com/sladg/apex-state/commit/5539fc3f8bc72b7bd78422c89675929673269e52))]
* Enforce functional programming: Convert all to arrow functions, eliminate classes [[eb05e60e946dd52eb67a7caee3175d901fb3bf54](https://github.com/sladg/apex-state/commit/eb05e60e946dd52eb67a7caee3175d901fb3bf54))]
* Complete: Enhanced Derived Values Testing - Reactivity Verification [[c7066950b3b4d83175d46035e9fa219d9f49f868](https://github.com/sladg/apex-state/commit/c7066950b3b4d83175d46035e9fa219d9f49f868))]
* Complete Phase 8: Comprehensive README documentation [[7656a9804a842e538f0463e80c17ba6d6f70b0b1](https://github.com/sladg/apex-state/commit/7656a9804a842e538f0463e80c17ba6d6f70b0b1))]
* Implement Phase 7: Integration Testing (simplified) [[91176a2b953015cde9ac2738101ee04565a64c63](https://github.com/sladg/apex-state/commit/91176a2b953015cde9ac2738101ee04565a64c63))]
* Implement Phase 6: Advanced Type Utilities and Form Hooks (APEX-5, 8, 17, 18) [[ced5c07769b25b89a04dac0aa3c28ad6f1f3fdbf](https://github.com/sladg/apex-state/commit/ced5c07769b25b89a04dac0aa3c28ad6f1f3fdbf))]
* Remove deprecated synchronizer placeholder functions [[ae61dbdae87a4067c30fee36c82795faeaa423fc](https://github.com/sladg/apex-state/commit/ae61dbdae87a4067c30fee36c82795faeaa423fc))]