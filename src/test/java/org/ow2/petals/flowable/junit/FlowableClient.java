/**
 * Copyright (c) 2015-2017 Linagora
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
package org.ow2.petals.flowable.junit;

import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import static org.ow2.petals.flowable.FlowableSEConstants.DBServer.DEFAULT_JDBC_DRIVER;

import java.io.File;
import java.net.MalformedURLException;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.logging.Logger;

import org.flowable.engine.HistoryService;
import org.flowable.engine.ProcessEngine;
import org.flowable.engine.ProcessEngineConfiguration;
import org.flowable.engine.RuntimeService;
import org.flowable.engine.TaskService;
import org.flowable.engine.impl.cfg.ProcessEngineConfigurationImpl;
import org.junit.Rule;
import org.junit.rules.ExternalResource;
import org.ow2.petals.flowable.identity.AbstractProcessEngineConfigurator;
import org.ow2.petals.flowable.identity.file.FileIdmEngineConfigurator;

/**
 * The {@link FlowableClient} {@link Rule} allows creation of a client connected to a Flowabl server. It is guaranteed
 * to be deleted when the test method finishes (whether it passes or fails):
 * 
 * <pre>
 * public class MyTestSuite {
 *  &#064;Rule
 *  public FlowableClient flowableClient = new FlowableClient(...);
 * 
 *  &#064;Test
 *  public void test...() throws IOException {
 *      ProcessInstance processInstance = flowableClient.getProcessInstance(processInstanceId);
 *      ...
 *  }
 * }
 * </pre>
 */
public class FlowableClient extends ExternalResource {

    private static final Logger LOG = Logger.getLogger(FlowableClient.class.getName());

    /**
     * The default JDBC username
     */
    public static final String DEFAULT_JDBC_USERNAME = "sa";

    /**
     * The default JDBC username
     */
    public static final String DEFAULT_JDBC_PWD = "";

    /**
     * The JDBC driver used to join the Flowable database
     */
    private final String jdbcDriver;

    /**
     * The JDBC Url locating the Flowable database
     */
    private final String jdbcUrl;

    /**
     * The JDBC username of the Flowable database
     */
    private final String jdbcUsername;

    /**
     * The JDBC password of the Flowable database
     */
    private final String jdbcPwd;

    /**
     * The configuration file of the IDM engine configurator
     */
    private final File idmEngineConfiguratorCfg;

    private ProcessEngine flowableClientEngine;

    /**
     * <p>
     * Creates a temporary Flowable client connected to a default Flowable server:
     * <ul>
     * <li>JDBC Driver: {@link #DEFAULT_JDBC_DRIVER},</li>
     * <li>JDBC URL: an embedded in-memoty H2 database,</li>
     * <li>JDBC username: {@link #DEFAULT_JDBC_USERNAME},</li>
     * <li>JDBC password: {@link #DEFAULT_JDBC_PWD}.</li>
     * </ul>
     * And using the default configuration for the identity service.
     * </p>
     */
    public FlowableClient() {
        this((String) null);
    }

    /**
     * <p>
     * Creates a temporary Flowable client connected to a default Flowable server:
     * <ul>
     * <li>JDBC Driver: {@link #DEFAULT_JDBC_DRIVER},</li>
     * <li>JDBC URL: an embedded in-memoty H2 database,</li>
     * <li>JDBC username: {@link #DEFAULT_JDBC_USERNAME},</li>
     * <li>JDBC password: {@link #DEFAULT_JDBC_PWD}.</li>
     * </ul>
     * </p>
     * 
     * @param idmEngineConfiguratorCfg
     *            Configuration file of the IDM engine configurator, as resource name
     */
    public FlowableClient(final String idmEngineConfiguratorCfg) {
        this(DEFAULT_JDBC_DRIVER, "jdbc:h2:mem:ativiti-test;DB_CLOSE_DELAY=-1", DEFAULT_JDBC_USERNAME, DEFAULT_JDBC_PWD,
                idmEngineConfiguratorCfg);
    }

    /**
     * <p>
     * Creates a temporary Flowable client connected to a default Flowable server:
     * <ul>
     * <li>JDBC Driver: {@link #DEFAULT_JDBC_DRIVER},</li>
     * <li>JDBC URL: an embedded in-memoty H2 database,</li>
     * <li>JDBC username: {@link #DEFAULT_JDBC_USERNAME},</li>
     * <li>JDBC password: {@link #DEFAULT_JDBC_PWD}.</li>
     * </ul>
     * </p>
     * 
     * @param idmEngineConfiguratorCfg
     *            Configuration file of the IDM engine configurator
     */
    public FlowableClient(final File idmEngineConfiguratorCfg) {
        this(DEFAULT_JDBC_DRIVER, "jdbc:h2:mem:ativiti-test;DB_CLOSE_DELAY=-1", DEFAULT_JDBC_USERNAME,
                DEFAULT_JDBC_PWD, idmEngineConfiguratorCfg);
    }

    /**
     * <p>
     * Creates a temporary Flowable client connected to a default Flowable server:
     * <ul>
     * <li>JDBC Driver: {@link #DEFAULT_JDBC_DRIVER},</li>
     * <li>JDBC URL: a JDBC URL for H2,</li>
     * <li>JDBC username: {@link #DEFAULT_JDBC_USERNAME},</li>
     * <li>JDBC password: {@link #DEFAULT_JDBC_PWD}.</li>
     * </ul>
     * </p>
     * 
     * @param h2JdbcUrl
     *            The JDBC URL for H2 database
     * @param idmEngineConfiguratorCfg
     *            Configuration file of the IDM engine configurator, as resource name
     */
    public FlowableClient(final String h2JdbcUrl, final String idmEngineConfiguratorCfg) {
        this(DEFAULT_JDBC_DRIVER, h2JdbcUrl.toString(), DEFAULT_JDBC_USERNAME, DEFAULT_JDBC_PWD,
                idmEngineConfiguratorCfg);
    }

    /**
     * <p>
     * Creates a temporary Flowable client connected to a default Flowable server:
     * <ul>
     * <li>JDBC Driver: {@link #DEFAULT_JDBC_DRIVER},</li>
     * <li>JDBC URL: an embedded H2 database as file,</li>
     * <li>JDBC username: {@link #DEFAULT_JDBC_USERNAME},</li>
     * <li>JDBC password: {@link #DEFAULT_JDBC_PWD}.</li>
     * </ul>
     * </p>
     * 
     * @param fileForJdbcUrl
     *            The file path of the Flowable database set as JDBC URL
     * @param idmEngineConfiguratorCfg
     *            Configuration file of the IDM engine configurator, as resource name
     */
    public FlowableClient(final File fileForJdbcUrl, final String idmEngineConfiguratorCfg) {
        this(DEFAULT_JDBC_DRIVER, String.format("jdbc:h2:%s", convertFile2Url(fileForJdbcUrl)), DEFAULT_JDBC_USERNAME,
                DEFAULT_JDBC_PWD, idmEngineConfiguratorCfg);
    }

    private static String convertFile2Url(final File fileForJdbcUrl) {
        try {
            return fileForJdbcUrl.toURI().toURL().toExternalForm();
        } catch (final MalformedURLException e) {
            fail(e.getMessage());
            return null;
        }
    }

    private static File convertUrl2File(final URL url) {
        try {
            return new File(url.toURI());
        } catch (final URISyntaxException e) {
            fail(e.getMessage());
            return null;
        }
    }

    /**
     * 
     * @param idmEngineConfiguratorCfg
     *            Configuration file of the IDM engine configurator, as resource name
     */
    public FlowableClient(final String jdbcDriver, final String jdbcUrl, final String jdbcUsername,
            final String jdbcPwd, final String idmEngineConfiguratorCfg) {

        this(jdbcDriver, jdbcUrl, jdbcUsername, jdbcPwd,
                idmEngineConfiguratorCfg == null ? null
                        : convertUrl2File(
                                Thread.currentThread().getContextClassLoader().getResource(idmEngineConfiguratorCfg)));
    }

    /**
     * 
     * @param idmEngineConfiguratorCfg
     *            Configuration file of the IDM engine configurator
     */
    public FlowableClient(final String jdbcDriver, final String jdbcUrl, final String jdbcUsername,
            final String jdbcPwd, final File idmEngineConfiguratorCfg) {
        this.jdbcDriver = jdbcDriver;
        this.jdbcUrl = jdbcUrl;
        this.jdbcUsername = jdbcUsername;
        this.jdbcPwd = jdbcPwd;
        this.idmEngineConfiguratorCfg = idmEngineConfiguratorCfg;
    }

    @Override
    protected void before() throws Throwable {
        this.create();
    }

    @Override
    protected void after() {
        try {
            this.delete();
        } catch (final Exception e) {
            e.printStackTrace();
        }
    }

    /**
     * Initialize & start the Flowable client
     */
    public void create() throws Exception {

        final ProcessEngineConfiguration pec = ProcessEngineConfiguration.createStandaloneProcessEngineConfiguration();
        pec.setJdbcDriver(this.jdbcDriver);
        pec.setJdbcUrl(this.jdbcUrl);
        pec.setJdbcUsername(this.jdbcUsername).setJdbcPassword(this.jdbcPwd);
        pec.setDatabaseSchemaUpdate("true");

        assertTrue(pec instanceof ProcessEngineConfigurationImpl);

        final AbstractProcessEngineConfigurator idmEngineConfigurator = new FileIdmEngineConfigurator();
        if (this.idmEngineConfiguratorCfg != null) {
            idmEngineConfigurator.setConfigurationFile(this.idmEngineConfiguratorCfg);
        }

        ((ProcessEngineConfigurationImpl) pec).setDisableIdmEngine(false);
        idmEngineConfigurator.setLogger(LOG);
        ((ProcessEngineConfigurationImpl) pec).setIdmProcessEngineConfigurator(idmEngineConfigurator);

        this.flowableClientEngine = pec.buildProcessEngine();
    }

    /**
     * Free the Flowable client, without freeing the Flowable database
     */
    public void delete() throws Exception {

        this.flowableClientEngine.close();
    }

    /**
     * @return The {@link RuntimeService} of the Flowable engine
     */
    public RuntimeService getRuntimeService() {
        return this.flowableClientEngine.getRuntimeService();
    }

    /**
     * @return The {@link TaskService} of the Flowable engine
     */
    public TaskService getTaskService() {
        return this.flowableClientEngine.getTaskService();
    }

    /**
     * @return The {@link HistoryService} of the Flowable engine
     */
    public HistoryService getHistoryService() {
        return this.flowableClientEngine.getHistoryService();
    }

    /**
     * @return The {@link IdentityService} of the Flowable engine
     */
    public org.flowable.engine.IdentityService getIdentityService() {
        return this.flowableClientEngine.getIdentityService();
    }

}