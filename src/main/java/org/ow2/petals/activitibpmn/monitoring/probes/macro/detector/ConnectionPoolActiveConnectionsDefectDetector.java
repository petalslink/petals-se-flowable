/**
 * Copyright (c) 2016-2017 Linagora
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
package org.ow2.petals.activitibpmn.monitoring.probes.macro.detector;

import org.apache.ibatis.datasource.pooled.PooledDataSource;
import org.ow2.petals.probes.api.sensor.detector.DefectCreator;
import org.ow2.petals.probes.api.sensor.detector.GaugeDefectDetector;

/**
 * Defect detector attached to the active connections of a {@link PooledDataSource}. A defect is created when no more
 * connection is available in the object pool.
 * 
 * @author Christophe DENEUX - Linagora
 */
public class ConnectionPoolActiveConnectionsDefectDetector implements GaugeDefectDetector<Long> {

    /**
     * The factory creating the defect about the connection pool exhausted.
     */
    private final DefectCreator connectionPoolExhaustedDefectCreator;

    /**
     * The connection pool
     */
    private PooledDataSource connectionPool = null;

    /**
     * 
     * @param connectionPoolExhaustedDefectCreator
     *            The factory creating and sending the defect about the connection pool exhausted
     */
    public ConnectionPoolActiveConnectionsDefectDetector(final DefectCreator connectionPoolExhaustedDefectCreator) {
        this.connectionPoolExhaustedDefectCreator = connectionPoolExhaustedDefectCreator;
    }

    public final void setConnectionPool(final PooledDataSource connectionPool) {
        this.connectionPool = connectionPool;
    }

    @Override
    public void detect(final Long instantValue) {
        if (this.connectionPool.getPoolState().getActiveConnectionCount() == this.connectionPool
                .getPoolMaximumActiveConnections()) {
            // No more connection is available
            this.connectionPoolExhaustedDefectCreator.createAndSend();
        }

    }
}
