/**
 * Copyright (c) 2016-2019 Linagora
 * 
 * This program/library is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or (at your
 * option) any later version.
 * 
 * This program/library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License
 * for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program/library; If not, see http://www.gnu.org/licenses/
 * for the GNU Lesser General Public License version 2.1.
 */
package org.ow2.petals.flowable.monitoring.probes.macro.sensor;

import org.apache.ibatis.datasource.pooled.PooledDataSource;
import org.ow2.petals.probes.api.sensor.GaugeSensor;

/**
 * Abstract sensor attached to the object pool '{@link PooledDataSource}'.
 * 
 * @author Christophe DENEUX - Linagora
 */
public abstract class AbstractObjectPoolGaugeSensor implements GaugeSensor<Long, Long> {

    /**
     * The connection pool
     */
    protected PooledDataSource connectionPool = null;

    @Override
    public final Long getInitialValue() {
        if (this.connectionPool == null) {
            return 0L;
        } else {
            return this.getInstantValue();
        }
    }

    @Override
    public final Long toExternalValue(final Long valueToConvert) {
        return valueToConvert;
    }

    public final void setConnectionPool(final PooledDataSource connectionPool) {
        this.connectionPool = connectionPool;
    }

}
