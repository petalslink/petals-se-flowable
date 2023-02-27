/**
 * Copyright (c) 2016-2023 Linagora
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
package org.ow2.petals.flowable.monitoring.probes.macro;

import org.apache.ibatis.datasource.pooled.PooledDataSource;
import org.ow2.petals.probes.api.exceptions.ProbeNotInitializedException;
import org.ow2.petals.probes.api.exceptions.ProbeNotStartedException;
import org.ow2.petals.probes.api.probes.Probe;

/**
 * A macro probe containing all probes needed to monitor an instance of the database connection pool
 * {@link PooledDataSource}.
 * 
 * @author Christophe DENEUX - Linagora
 * 
 */
public interface PooledDataSourceProbe extends Probe {

    /**
     * Set the database connection pool to probe
     * 
     * @param connectionPool
     *            the database connection pool to probe
     */
    public void setConnectionPool(final PooledDataSource connectionPool);

    /**
     * @return The current max size of connections that the database connection pool can accept.
     */
    public long getConnectionPoolMaxSize();

    /**
     * @return The current max size of idle connections that the database connection pool can accept.
     */
    public long getConnectionPoolMaxIdleSize();

    /**
     * @return The max number of active connections of the database connection pool
     */
    public long getConnectionPoolActiveConnectionsMax() throws ProbeNotInitializedException;

    /**
     * @return The current number of active connections of the database connection pool
     */
    public long getConnectionPoolActiveConnectionsCurrent() throws ProbeNotStartedException;

    /**
     * @return The max number of idle connections of the database connection pool
     */
    public long getConnectionPoolIdleConnectionsMax() throws ProbeNotInitializedException;

    /**
     * @return The current number of idle connections of the object pool
     */
    public long getConnectionPoolIdleConnectionsCurrent() throws ProbeNotStartedException;

    /**
     * Realizes a measure of the connection pool:
     * <ul>
     * <li>current number of allocated connections,</li>
     * <li>current number of enqueued tasks.</li>
     * </ul>
     */
    public void pick() throws ProbeNotStartedException;

}
