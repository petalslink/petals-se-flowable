/**
 * Copyright (c) 2016-2022 Linagora
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
package org.ow2.petals.flowable.monitoring.probes.macro.impl;

import org.apache.ibatis.datasource.pooled.PooledDataSource;
import org.ow2.petals.flowable.monitoring.probes.macro.PooledDataSourceProbe;
import org.ow2.petals.flowable.monitoring.probes.macro.detector.ConnectionPoolActiveConnectionsDefectDetector;
import org.ow2.petals.flowable.monitoring.probes.macro.sensor.ConnectionPoolActiveConnectionsGaugeSensor;
import org.ow2.petals.flowable.monitoring.probes.macro.sensor.ConnectionPoolIdleConnectionsGaugeSensor;
import org.ow2.petals.probes.api.ProbesFactory;
import org.ow2.petals.probes.api.ProbesFactoryBuilder;
import org.ow2.petals.probes.api.exceptions.MultipleProbesFactoriesFoundException;
import org.ow2.petals.probes.api.exceptions.NoProbesFactoryFoundException;
import org.ow2.petals.probes.api.exceptions.ProbeInitializationException;
import org.ow2.petals.probes.api.exceptions.ProbeInitializedException;
import org.ow2.petals.probes.api.exceptions.ProbeNotInitializedException;
import org.ow2.petals.probes.api.exceptions.ProbeNotStartedException;
import org.ow2.petals.probes.api.exceptions.ProbeShutdownException;
import org.ow2.petals.probes.api.exceptions.ProbeStartedException;
import org.ow2.petals.probes.api.exceptions.ProbeStartupException;
import org.ow2.petals.probes.api.exceptions.ProbeStopException;
import org.ow2.petals.probes.api.probes.GaugeProbe;
import org.ow2.petals.probes.api.sensor.detector.DefectCreator;

/**
 * <p>
 * A macro containing all probes needed to monitor a {@link PooledDataSource}
 * </p>
 * <p>
 * <u><b>Lifecycle</b>:</u> The instant value can be acceded or picked only if the probe is started. The max value can
 * be acceded even if the probe is initialized or stopped. The max value is reseted to the sensor initial value when the
 * probe starts.
 * </p>
 * 
 * @author Christophe DENEUX - Linagora
 * 
 */
public class PooledDataSourceProbeImpl implements PooledDataSourceProbe {

    /**
     * The probe counting active connections in the database connection pool.
     */
    private GaugeProbe<Long, Long> probeConnectionPoolActiveConnections;

    /**
     * The probe counting idle connections of the database connection pool.
     */
    private GaugeProbe<Long, Long> probeConnectionPoolIdleConnections;

    /**
     * Sensor attached to the active connections of the connection pool.
     */
    private final ConnectionPoolActiveConnectionsGaugeSensor connectionPoolActiveConnectionsGaugeSensor;

    /**
     * Defect detector attached to the active connections of the connection pool.
     */
    private final ConnectionPoolActiveConnectionsDefectDetector connectionPoolActiveConnectionsDefectDetector;

    /**
     * Sensor attached to the idle connections of the database connection pool.
     */
    private final ConnectionPoolIdleConnectionsGaugeSensor connectionPoolIdleConnectionsGaugeSensor;

    /**
     * The probed connection pool
     */
    private PooledDataSource connectionPool = null;

    /**
     * @param connectionPoolActiveConnectionsDefectCreator
     *            Defect creator used to create defects about exhaustion of the connection pool
     * @throws NoProbesFactoryFoundException
     *             No probe implementation found in the classloader
     * @throws MultipleProbesFactoriesFoundException
     *             Several probe implementations found in the classloader
     */
    public PooledDataSourceProbeImpl(final DefectCreator connectionPoolActiveConnectionsDefectCreator)
            throws MultipleProbesFactoriesFoundException, NoProbesFactoryFoundException {

        final ProbesFactoryBuilder<Long, Long> probesFactoryBuilder = new ProbesFactoryBuilder<>();
        final ProbesFactory<Long, Long> probesFactory = probesFactoryBuilder.getProbesFactory();

        // Create the probe about active connections
        this.connectionPoolActiveConnectionsGaugeSensor = new ConnectionPoolActiveConnectionsGaugeSensor();
        this.connectionPoolActiveConnectionsDefectDetector = new ConnectionPoolActiveConnectionsDefectDetector(
                connectionPoolActiveConnectionsDefectCreator);
        this.probeConnectionPoolActiveConnections = probesFactory.createGaugeProbe(
                this.connectionPoolActiveConnectionsGaugeSensor, this.connectionPoolActiveConnectionsDefectDetector);

        // Create the probe about idle connections
        this.connectionPoolIdleConnectionsGaugeSensor = new ConnectionPoolIdleConnectionsGaugeSensor();
        this.probeConnectionPoolIdleConnections = probesFactory
                .createGaugeProbe(this.connectionPoolIdleConnectionsGaugeSensor, null);

    }

    @Override
    public void setConnectionPool(final PooledDataSource connectionPool) {
        this.connectionPool = connectionPool;
        this.connectionPoolActiveConnectionsDefectDetector.setConnectionPool(this.connectionPool);
        this.connectionPoolActiveConnectionsGaugeSensor.setConnectionPool(this.connectionPool);
        this.connectionPoolIdleConnectionsGaugeSensor.setConnectionPool(this.connectionPool);
    }

    @Override
    public long getConnectionPoolMaxSize() {
        if (this.connectionPool == null) {
            return 0L;
        } else {
            return this.connectionPool.getPoolMaximumActiveConnections();
        }
    }

    @Override
    public long getConnectionPoolMaxIdleSize() {
        if (this.connectionPool == null) {
            return 0L;
        } else {
            return this.connectionPool.getPoolMaximumIdleConnections();
        }
    }

    @Override
    public long getConnectionPoolActiveConnectionsMax() throws ProbeNotInitializedException {
        return this.probeConnectionPoolActiveConnections.getMaxValue();
    }

    @Override
    public long getConnectionPoolActiveConnectionsCurrent() throws ProbeNotStartedException {
        return this.probeConnectionPoolActiveConnections.getInstantValue();
    }

    @Override
    public long getConnectionPoolIdleConnectionsMax() throws ProbeNotInitializedException {
        return this.probeConnectionPoolIdleConnections.getMaxValue();
    }

    @Override
    public long getConnectionPoolIdleConnectionsCurrent() throws ProbeNotStartedException {
        return this.probeConnectionPoolIdleConnections.getInstantValue();
    }

    @Override
    public void pick() throws ProbeNotStartedException {
        this.probeConnectionPoolActiveConnections.pick();
        this.probeConnectionPoolIdleConnections.pick();
    }

    @Override
    public void init() throws ProbeInitializedException, ProbeStartedException, ProbeInitializationException {
        this.probeConnectionPoolActiveConnections.init();
        this.probeConnectionPoolIdleConnections.init();
    }

    @Override
    public void start() throws ProbeNotInitializedException, ProbeStartedException, ProbeStartupException {
        this.probeConnectionPoolActiveConnections.start();
        this.probeConnectionPoolIdleConnections.start();
    }

    @Override
    public void stop() throws ProbeNotInitializedException, ProbeNotStartedException, ProbeStopException {
        this.probeConnectionPoolActiveConnections.stop();
        this.probeConnectionPoolIdleConnections.stop();
    }

    @Override
    public void shutdown() throws ProbeShutdownException, ProbeStartedException, ProbeNotInitializedException {
        this.probeConnectionPoolActiveConnections.shutdown();
        this.probeConnectionPoolIdleConnections.shutdown();
    }

}
