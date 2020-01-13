/**
 * Copyright (c) 2016-2020 Linagora
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
package org.ow2.petals.flowable.monitoring;

import java.util.List;
import java.util.Timer;
import java.util.concurrent.ThreadPoolExecutor;

import javax.management.openmbean.CompositeDataSupport;
import javax.management.openmbean.CompositeType;
import javax.management.openmbean.OpenDataException;
import javax.management.openmbean.OpenType;
import javax.management.openmbean.SimpleType;
import javax.management.openmbean.TabularData;
import javax.management.openmbean.TabularDataSupport;
import javax.management.openmbean.TabularType;

import org.apache.ibatis.datasource.pooled.PooledDataSource;
import org.flowable.engine.HistoryService;
import org.flowable.engine.ProcessEngine;
import org.flowable.engine.RepositoryService;
import org.flowable.engine.RuntimeService;
import org.flowable.engine.history.HistoricProcessInstance;
import org.flowable.engine.repository.ProcessDefinition;
import org.flowable.engine.runtime.ProcessInstance;
import org.ow2.petals.component.framework.clientserver.api.monitoring.exception.MonitoringProbeNotInitializedException;
import org.ow2.petals.component.framework.clientserver.api.monitoring.exception.MonitoringProbeNotStartedException;
import org.ow2.petals.component.framework.clientserver.api.monitoring.exception.MonitoringServiceException;
import org.ow2.petals.component.framework.monitoring.defect.JmxDefectCreator;
import org.ow2.petals.flowable.monitoring.defect.AsyncExecutorThreadPoolDefectCreator;
import org.ow2.petals.flowable.monitoring.defect.PooledDataSourceConnectionPoolDefectCreator;
import org.ow2.petals.flowable.monitoring.probes.macro.PooledDataSourceProbe;
import org.ow2.petals.flowable.monitoring.probes.macro.impl.PooledDataSourceProbeImpl;
import org.ow2.petals.probes.api.MacroProbesFactory;
import org.ow2.petals.probes.api.MacroProbesFactoryBuilder;
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
import org.ow2.petals.probes.api.probes.macro.ThreadPoolProbe;

/**
 * The monitoring MBean of the SE Flowable
 *
 * @author Christophe DENEUX - Linagora
 */
public class Monitoring extends org.ow2.petals.component.framework.monitoring.Monitoring implements MonitoringMBean {

    /**
     * Item names of the process definition TabularData returned by {@link #getProcessDefinitions()}
     */
    private static final String[] PROCESS_DEFINITIONS_ITEM_NAMES;

    /**
     * TabularData type of the process definition returned by {@link #getProcessDefinitions()}
     */
    private static final TabularType PROCESS_DEFINITIONS_TABULAR_TYPE;

    /**
     * The repository service of the Flowable engine
     */
    private RepositoryService repositoryService = null;

    /**
     * The runtime service of the Flowable engine
     */
    private RuntimeService runtimeService = null;

    /**
     * The history service of the Flowable engine
     */
    private HistoryService historyService = null;

    // --- Probes --- //

    /**
     * The macro probe about the thread pool of the async executor.
     */
    private final ThreadPoolProbe probeAsyncExecutorThreadPool;

    /**
     * The macro probe about the database connection pool.
     */
    private final PooledDataSourceProbe probeDatabaseConnectionPool;

    /**
     * Creates the monitoring MBean
     *
     * @param responseTimeProbesTimer
     *            The timer used as sampler by response time probes. <b>The caller is responsible to cancel the timer to
     *            free resources attached</b>.
     * @param samplePeriod
     *            The duration of a sample, in milliseconds.
     * @throws NoProbesFactoryFoundException
     *             No probe implementation found in the classloader
     * @throws MultipleProbesFactoriesFoundException
     *             Several probe implementations found in the classloader
     */
    public Monitoring(final Timer responseTimeProbesTimer, final long samplePeriod)
            throws MultipleProbesFactoriesFoundException, NoProbesFactoryFoundException {

        super(responseTimeProbesTimer, samplePeriod);

        final MacroProbesFactoryBuilder macroProbesFactoryBuilder = new MacroProbesFactoryBuilder();
        final MacroProbesFactory macroProbesFactory = macroProbesFactoryBuilder.getMacroProbesFactory();

        final JmxDefectCreator asyncExecutorThreadPoolDefectCreator = new AsyncExecutorThreadPoolDefectCreator(this);
        this.probeAsyncExecutorThreadPool = macroProbesFactory
                .createThreadPoolProbe(asyncExecutorThreadPoolDefectCreator);

        final JmxDefectCreator connectionPoolActiveConnectionsDefectCreator = new PooledDataSourceConnectionPoolDefectCreator(
                this);
        this.probeDatabaseConnectionPool = new PooledDataSourceProbeImpl(connectionPoolActiveConnectionsDefectCreator);
    }

    /**
     * @param flowableEngine
     *            The Flowable BPMN Engine
     */
    public void setFlowableEngine(final ProcessEngine flowableEngine) {
        this.repositoryService = flowableEngine.getRepositoryService();
        this.runtimeService = flowableEngine.getRuntimeService();
        this.historyService = flowableEngine.getHistoryService();
    }

    /**
     * @param asyncExecutorTreadPool
     *            The asynchronous executor thread pool to monitor
     */
    public void setAsyncExecutorTreadPool(final ThreadPoolExecutor asyncExecutorTreadPool) {
        this.probeAsyncExecutorThreadPool.setThreadPool(asyncExecutorTreadPool);
    }

    /**
     * @param dataSourcePool
     *            The database connection pool to monitor
     */
    public void setDataSourcePool(final PooledDataSource dataSourcePool) {
        this.probeDatabaseConnectionPool.setConnectionPool(dataSourcePool);
    }

    @Override
    public void doInit() throws ProbeInitializedException, ProbeStartedException, ProbeInitializationException {
        this.probeAsyncExecutorThreadPool.init();
        this.probeDatabaseConnectionPool.init();
    }

    @Override
    protected void doStart() throws ProbeNotInitializedException, ProbeStartedException, ProbeStartupException {
        this.probeAsyncExecutorThreadPool.start();
        this.probeDatabaseConnectionPool.start();
    }

    @Override
    public void doStop() throws ProbeNotInitializedException, ProbeNotStartedException, ProbeStopException {
        this.probeAsyncExecutorThreadPool.stop();
        this.probeDatabaseConnectionPool.stop();
    }

    @Override
    public void doShutdown() throws ProbeShutdownException, ProbeStartedException, ProbeNotInitializedException {
        this.probeAsyncExecutorThreadPool.shutdown();
        this.probeDatabaseConnectionPool.shutdown();
    }

    @Override
    public TabularData getProcessDefinitions() throws OpenDataException {

        assert PROCESS_DEFINITIONS_TABULAR_TYPE != null;
        final TabularDataSupport tabServiceInvocations = new TabularDataSupport(PROCESS_DEFINITIONS_TABULAR_TYPE);

        final List<ProcessDefinition> deployments = this.repositoryService.createProcessDefinitionQuery().list();

        for (final ProcessDefinition deployment : deployments) {
            final Object[] values = new Object[5];

            // Process definition
            values[PROCESS_DEFINITION_INFO_ID_KEY] = deployment.getKey();

            // Suspension state
            values[PROCESS_DEFINITION_INFO_ID_SUSPENSION_STATE] = deployment.isSuspended() ? 1L : 0L;

            // active process instances
            final List<ProcessInstance> activeProcessInstances = this.runtimeService.createProcessInstanceQuery()
                    .processDefinitionKey(deployment.getKey()).active().list();
            values[PROCESS_DEFINITION_INFO_ID_ACTIVE_INSTANCES_COUNTER] = Long.valueOf(activeProcessInstances.size());

            // suspended process instances
            final List<ProcessInstance> suspendedProcessInstances = this.runtimeService.createProcessInstanceQuery()
                    .processDefinitionKey(deployment.getKey()).suspended().list();
            values[PROCESS_DEFINITION_INFO_ID_SUSPENDED_INSTANCES_COUNTER] = Long
                    .valueOf(suspendedProcessInstances.size());

            // ended process instances
            final List<HistoricProcessInstance> endedProcessInstances = this.historyService
                    .createHistoricProcessInstanceQuery().processDefinitionKey(deployment.getKey()).finished().list();
            values[PROCESS_DEFINITION_INFO_ID_ENDED_INSTANCES_COUNTER] = Long.valueOf(endedProcessInstances.size());

            final CompositeDataSupport support = new CompositeDataSupport(
                    PROCESS_DEFINITIONS_TABULAR_TYPE.getRowType(),
                    PROCESS_DEFINITIONS_ITEM_NAMES,
                    values);

            tabServiceInvocations.put(support);
        }
        return tabServiceInvocations;
    }

    //
    // --- Message exchange processor thread pool metrics
    //

    @Override
    public long getAsyncExecutorThreadPoolMaxSize() throws MonitoringServiceException {
        return this.probeAsyncExecutorThreadPool.getThreadPoolMaxSize();
    }

    @Override
    public long getAsyncExecutorThreadPoolMinSize() throws MonitoringServiceException {
        return this.probeAsyncExecutorThreadPool.getThreadPoolMinSize();
    }

    @Override
    public long getAsyncExecutorThreadPoolActiveThreadsMax()
            throws MonitoringProbeNotInitializedException, MonitoringServiceException {
        try {
            return this.probeAsyncExecutorThreadPool.getThreadPoolActiveThreadsMax();
        } catch (final ProbeNotInitializedException e) {
            throw new MonitoringProbeNotInitializedException(e);
        }
    }

    @Override
    public long getAsyncExecutorThreadPoolActiveThreadsCurrent()
            throws MonitoringProbeNotStartedException, MonitoringServiceException {
        try {
            return this.probeAsyncExecutorThreadPool.getThreadPoolActiveThreadsCurrent();
        } catch (final ProbeNotStartedException e) {
            throw new MonitoringProbeNotStartedException(e);
        }
    }

    @Override
    public long getAsyncExecutorThreadPoolIdleThreadsMax()
            throws MonitoringProbeNotInitializedException, MonitoringServiceException {
        try {
            return this.probeAsyncExecutorThreadPool.getThreadPoolIdleThreadsMax();
        } catch (final ProbeNotInitializedException e) {
            throw new MonitoringProbeNotInitializedException(e);
        }
    }

    @Override
    public long getAsyncExecutorThreadPoolIdleThreadsCurrent()
            throws MonitoringProbeNotStartedException, MonitoringServiceException {
        try {
            return this.probeAsyncExecutorThreadPool.getThreadPoolIdleThreadsCurrent();
        } catch (final ProbeNotStartedException e) {
            throw new MonitoringProbeNotInitializedException(e);
        }
    }

    @Override
    public long getAsyncExecutorThreadPoolQueuedRequestsMax()
            throws MonitoringProbeNotInitializedException, MonitoringServiceException {
        try {
            return this.probeAsyncExecutorThreadPool.getThreadPoolQueuedRequestsMax();
        } catch (final ProbeNotInitializedException e) {
            throw new MonitoringProbeNotInitializedException(e);
        }
    }

    @Override
    public long getAsyncExecutorThreadPoolQueuedRequestsCurrent()
            throws MonitoringProbeNotStartedException, MonitoringServiceException {
        try {
            return this.probeAsyncExecutorThreadPool.getThreadPoolQueuedRequestsCurrent();
        } catch (final ProbeNotStartedException e) {
            throw new MonitoringProbeNotStartedException(e);
        }
    }

    public ThreadPoolProbe getProbeAsyncExecutorThreadPool() {
        return this.probeAsyncExecutorThreadPool;
    }

    public PooledDataSourceProbe getProbeDatabaseConnectionPool() {
        return this.probeDatabaseConnectionPool;
    }

    @Override
    public long getDatabaseConnectionPoolMaxActiveSize() throws MonitoringServiceException {
        return this.probeDatabaseConnectionPool.getConnectionPoolMaxSize();
    }

    @Override
    public long getDatabaseConnectionPoolMaxIdleSize() throws MonitoringServiceException {
        return this.probeDatabaseConnectionPool.getConnectionPoolMaxIdleSize();
    }

    @Override
    public long getDatabaseConnectionPoolActiveConnectionsMax()
            throws MonitoringProbeNotInitializedException, MonitoringServiceException {
        try {
            return this.probeDatabaseConnectionPool.getConnectionPoolActiveConnectionsMax();
        } catch (final ProbeNotInitializedException e) {
            throw new MonitoringProbeNotStartedException(e);
        }
    }

    @Override
    public long getDatabaseConnectionPoolActiveConnectionsCurrent()
            throws MonitoringProbeNotStartedException, MonitoringServiceException {
        try {
            return this.probeDatabaseConnectionPool.getConnectionPoolActiveConnectionsCurrent();
        } catch (final ProbeNotStartedException e) {
            throw new MonitoringProbeNotStartedException(e);
        }
    }

    @Override
    public long getDatabaseConnectionPoolIdleConnectionsMax()
            throws MonitoringProbeNotInitializedException, MonitoringServiceException {
        try {
            return this.probeDatabaseConnectionPool.getConnectionPoolIdleConnectionsMax();
        } catch (final ProbeNotInitializedException e) {
            throw new MonitoringProbeNotStartedException(e);
        }
    }

    @Override
    public long getDatabaseConnectionPoolIdleConnectionsCurrent()
            throws MonitoringProbeNotStartedException, MonitoringServiceException {
        try {
            return this.probeDatabaseConnectionPool.getConnectionPoolIdleConnectionsCurrent();
        } catch (final ProbeNotStartedException e) {
            throw new MonitoringProbeNotStartedException(e);
        }
    }

    static {
        try {
            //
            // --- Process definitions
            //
            PROCESS_DEFINITIONS_ITEM_NAMES = new String[] {
                    "processDefinition", "suspensionState", "active", "suspended", "ended"};

            final CompositeType processDefinitionsCompositeType = new CompositeType(
                    "ProcessDefinition",
                    "current process definitions deployed with some metrics on process instances",
                    PROCESS_DEFINITIONS_ITEM_NAMES,
                    new String[] {
                            "definition of the process",
                            "the suspension state: '1' the process definition is suspended, '0' it is not suspended",
                            "the number of active process instances",
                            "the number of suspended process instances",
                    "the number of ended process instances"},
                    new OpenType[] { SimpleType.STRING, SimpleType.LONG, SimpleType.LONG, SimpleType.LONG,
                            SimpleType.LONG });

            PROCESS_DEFINITIONS_TABULAR_TYPE = new TabularType(
                    "ProcessDefinitions",
                    "List current process definitions deployed with some metrics on process instances",
                    processDefinitionsCompositeType,
                    new String[] {"processDefinition"});

        } catch (final OpenDataException e) {
            throw new RuntimeException(e);
        }

    }
}
