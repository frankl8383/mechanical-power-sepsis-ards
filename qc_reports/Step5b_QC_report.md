# Step 5b — eICU 外部验证 QC 报告

> 判定:**PASS**(附条件披露:结局口径 + plateau 压记录稀疏)
> 生成于 2026-07-04 · 环境 icu-vent (R 4.5.3 + data.table + survival + pROC)

---

## 一、任务与判定

把 MIMIC-IV 冻结主模型(`models_step5.rds$primary`,logistic:
`died_28d ~ MP_baseline + dP_baseline + anchor_age + gender + pf_day1_min`)
套到独立构建的 **eICU-CRD v2.0 脓毒症-ARDS 通气队列**,报外部判别力、校准、
recalibration-in-the-large、敏感性矩阵。

**核心结论:外部验证成功。** 判别力几乎完全迁移(外部 C=0.656 vs MIMIC 内部
0.664);绝对风险因结局口径差异需 recalibration-in-the-large,但校准斜率
0.865(CI 跨 1)表明各暴露相对权重在 eICU 依然成立。

---

## 二、队列构建核查

| 漏斗层 | N | 保留率 | 核查 |
|---|---|---|---|
| L0 全部 ICU stay | 200,859 | — | patient 表全量 ✓ |
| L1 成人 (age≥16) | 200,583 | 99.9% | '> 89'→90 映射 ✓ |
| L2 + 首个 ICU stay | 158,189 | 78.9% | unitvisitnumber==1 ✓ |
| L3 + 机械通气 | 61,991 | 39.2% | 三来源并集(respChart 参数∪respCare∪apacheVent) |
| L4 + 脓毒症 | 10,937 | 17.6% | diagnosisstring 含 sepsis/septic |
| L5 + ARDS (P/F≤300) | 7,959 | 72.8% | PaO2/FiO2 ±2h 配对,同 MIMIC |

- **通气识别**:MIMIC 用"有通气 itemid",eICU 用同构的"有通气参数/ventstart/apache vent 标记"。✓
- **保留率合理**:通气 39%、脓毒症 17.6%、ARDS 72.8% 均符合临床预期。✓

## 三、暴露口径核查(关键:逐字对齐 MIMIC)

MIMIC 公式(取自 `trajectory_features.rds` lineage,逐字复现):
```
peep_use = peep(优先)/ peep_tot(补)   [eICU 无 total PEEP,用 PEEP]
dP  = plat − peep_use                 截断 [0,40] 否则 NA
MP  = 0.098 × rr × (tv/1000) × (ppeak − 0.5·dP)   截断 [0,100] 否则 NA
合理区间: plat[5,60] peep[0,30] tv[50,1500] ppeak[5,80] rr[1,60] fio2[21,100]
```

| 暴露 | eICU (mean±SD) | MIMIC (mean±SD) | 可比性 |
|---|---|---|---|
| MP (J/min) | 15.36±7.60 | 14.03±6.79 | 优(eICU 略高 1.3) |
| ΔP (cmH₂O) | 14.12±4.94 | 12.27±3.54 | 优(eICU 略高 1.8) |
| P/F (Day-1 strict) | 155.3±92.9 | 150.4±93.5 | 优(几乎重合) |
| Age (yr) | 63.6±15.2 | 63.7±14.9 | 完美一致 |
| tv/PBW (mL/kg) | 7.54±1.39 | 7.55±1.47 | 完美一致 |
| Male | 55.4% | 61.9% | 可比 |

- **P/F 口径修正记录(诚信)**:初版对无 Day-1 P/F 者回填全程最低值(混合口径)。
  已修正:主分析改用**严格 Day-1 P/F**(与 MIMIC 同口径);完全病例内 96.4% 本就是
  严格 Day-1(仅 69/1917=3.6% 回填),两口径分布几乎相同(155.26 vs 155.59)。
  回填版保留为敏感性分析(C=0.657,与主分析 0.656 一致)。
- **分布图口径一致性修正(诚信)**:初版 4 面板中 MP/ΔP/Age 三面板沿用 n=1,917
  直方图而图例标 n=1,848。已修正:全部 4 面板直方图统一自 n=1,848 暴露完全集重生成。

## 四、⚠️ 已知限制(必须在稿件披露)

1. **结局口径差异(最重要)**:eICU 无出院后随访,无法复现"28 天全因死亡"。
   主结局改为**院内死亡**(用户 2026-07-04 确认)。eICU 院内死亡率 33.2% > MIMIC
   28天死亡率 20.8%(口径差异 + 完全病例为记录了 plateau 压的更重患者=选择偏倚)。
   → 直接导致 CITL=0.539(系统低估),需 recalibration-in-the-large。**已如实呈现。**
2. **plateau 压记录稀疏**:eICU respiratoryCharting 中 Plateau Pressure 仅
   372K 行,导致 MP/ΔP 暴露完全病例从 7,959 降至 1,848(23.2%);主分析集(再加
   结局+协变量齐全)N=1,837(23.1%)。事件数 610 足够
   (EPV≫10),但样本量限制外部验证精度(C 的 CI 宽度 ±0.027)。
3. **脓毒症定义**:eICU 无 MIMIC 式 inputevents 抗生素时间戳,用 diagnosisstring
   诊断编码近似 Seymour 标准(非严格培养+抗生素配对)。已在 Methods 披露。

## 五、结果稳健性(敏感性矩阵)

| 分析 | N | 事件 | 外部C | 校准斜率 |
|---|---|---|---|---|
| 主分析(院内死亡) | 1,837 | 610 | 0.656 (0.630–0.683) | 0.865 |
| ICU死亡(更严结局) | 1,837 | 504 | 0.662 | 0.882 |
| P/F 回填(混合口径) | 1,906 | 628 | 0.657 | 0.874 |
| ARDS Severe (P/F≤100, Berlin) | 644 | 278 | 0.648 | 0.926 |
| ARDS Moderate (100–200, Berlin) | 678 | 197 | 0.641 | 0.955 |
| ARDS Mild (200–300, Berlin) | 383 | 101 | 0.620 | 0.841 |

- **外部 C 跨 6 设定稳定于 0.62–0.66**,与主分析高度一致。✓
- ICU死亡 C=0.662 略高、P/F 回填 C=0.657 几乎相同 → 判别力对结局定义和 P/F 口径
  均稳健。✓
- **ARDS 严重度用显式 Berlin 边界**(Mild 封顶 300)。注:pf_day1_min 为 Day-1 最低值,
  132 例 Day-1 最低 P/F>300(入队基于全程任意 P/F≤300),落在 Berlin 三分层外(NA),
  故 Mild 亚组 N=383 而非全体减 Severe+Moderate。校准十分位观测率总体上升
  (0.18→0.58)但非严格单调(第 4/5 十分位略低于第 3),不影响校准结论。

## 六、数据外泄纪律核查

- **仅导出聚合统计量**:funnel 计数、分布 mean/SD/分位、直方图 bin 密度、C/校准/
  Brier、敏感性 C。**零行级数据导出。** ✓
- 原始 eICU 数据全程只读(`/secure_data/eicu-crd/2.0`)。✓

## 七、方法学纪律继承核查(交接文档五条)

1. ✅ 主模型用 logistic(同 MIMIC:Cox PH 违反 + 时间列坏钟)
2. ✅ 暴露用 Day-1 基线(无斜率/峰值/AUC,避免不朽时间偏倚)
3. ✅ MP/ΔP 逐字复现 MIMIC 公式
4. ✅ 结局口径差异如实披露 + recalibration
5. ✅ 仅导聚合统计量

**判定:PASS**(附三条限制披露)
