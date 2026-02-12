/**
 * Copyright (c) 2015-2026 Linagora
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
package org.ow2.petals.flowable.junit.extensions.impl;

import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.fail;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.net.MalformedURLException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Comparator;
import java.util.Properties;
import java.util.logging.Logger;

import org.flowable.engine.ProcessEngine;
import org.flowable.engine.ProcessEngineConfiguration;
import org.flowable.engine.impl.cfg.ProcessEngineConfigurationImpl;
import org.junit.jupiter.api.extension.ExtensionContext.Store.CloseableResource;
import org.ow2.petals.flowable.identity.SeFlowableIdmEngineConfigurator;
import org.ow2.petals.flowable.identity.file.FileIdmEngineConfigurator;
import org.ow2.petals.flowable.junit.extensions.api.FlowableClient;

public class FlowableClientImpl implements FlowableClient, CloseableResource {

    private static final Logger LOG = Logger.getLogger(FlowableClientImpl.class.getName());

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
     * The IDM engine configurator to use
     */
    private final SeFlowableIdmEngineConfigurator idmEngineConfigurator;

    /**
     * The folder that will be used as the root of the file-system used by the Flowable client, and in which file-based
     * database and IDM engine files will be stored.
     */
    private final Path rootFileSystem;

    /**
     * If {@code true}, the root of the file-system used by the Flowable client will be cleaned when the Flowable client
     * will be closed
     */
    private final boolean rootFileSystemToClean;

    /**
     * The configuration file of the IDM engine configurator
     */
    private File idmEngineConfiguratorCfgFile;

    private ProcessEngineConfiguration processEngineConfiguration;

    private ProcessEngine flowableClientEngine;

    /**
     * Create a Flowable client based on a database requiring no file.
     * 
     * @param idmEngineConfigurator
     *            IDM engine configurator
     * @param idmEngineConfiguratorCfgFile
     *            Configuration file of the IDM engine configurator
     */
    public FlowableClientImpl(final String jdbcDriver, final String jdbcUrlPattern, final String databaseName,
            final String jdbcUsername, final String jdbcPwd,
            final SeFlowableIdmEngineConfigurator idmEngineConfigurator, final File idmEngineConfiguratorCfgFile)
            throws IOException {
        this(jdbcDriver, String.format(jdbcUrlPattern, databaseName), jdbcUsername, jdbcPwd, idmEngineConfigurator,
                idmEngineConfiguratorCfgFile, Files.createTempDirectory("flowable-client"), false);
    }

    /**
     * Create a Flowable client based on a database requiring no file.
     * 
     * @param rootFileSystem
     *            The folder that will be used as the root of the file-system used by the Flowable client, and in which
     *            file-based database will be stored. If {@code null}, it will be automatically deleted when closing the
     *            Flowable client instance.
     * @param idmEngineConfigurator
     *            IDM engine configurator
     * @param idmEngineConfiguratorCfg
     *            Configuration file of the IDM engine configurator
     */
    public FlowableClientImpl(final String jdbcDriver, final String jdbcUrlPattern, final Path rootFileSystem,
            final String databaseFileName, final String jdbcUsername, final String jdbcPwd,
            final SeFlowableIdmEngineConfigurator idmEngineConfigurator, final File idmEngineConfiguratorCfg)
            throws IOException {
        this(jdbcDriver,
                String.format(jdbcUrlPattern,
                        convertPath2Url(getRootFileSystemToUse(rootFileSystem).resolve(databaseFileName))),
                jdbcUsername, jdbcPwd, idmEngineConfigurator, idmEngineConfiguratorCfg,
                getRootFileSystemToUse(rootFileSystem), rootFileSystem == null ? true : false);
    }

    private static Path getRootFileSystemToUse(final Path rootFileSystem) throws IOException {
        return rootFileSystem == null ? Files.createTempDirectory("flowable-client") : rootFileSystem;
    }

    private static String convertPath2Url(final Path path) {
        try {
            return path.toUri().toURL().toExternalForm();
        } catch (final MalformedURLException e) {
            fail(e.getMessage());
            return null;
        }
    }

    /**
     * Create a Flowable client based on a database requiring no file.
     * 
     * @param idmEngineConfigurator
     *            IDM engine configurator
     * @param idmEngineConfiguratorCfgFile
     *            Configuration file of the IDM engine configurator
     */
    private FlowableClientImpl(final String jdbcDriver, final String jdbcUrl, final String jdbcUsername,
            final String jdbcPwd, final SeFlowableIdmEngineConfigurator idmEngineConfigurator,
            final File idmEngineConfiguratorCfgFile, final Path rootFileSystem, final boolean rootFileSystemToClean) {
        this.jdbcDriver = jdbcDriver;
        this.jdbcUrl = jdbcUrl;
        this.jdbcUsername = jdbcUsername;
        this.jdbcPwd = jdbcPwd;
        this.rootFileSystem = rootFileSystem;
        this.rootFileSystemToClean = rootFileSystemToClean;
        this.idmEngineConfigurator = idmEngineConfigurator;
        this.idmEngineConfiguratorCfgFile = idmEngineConfiguratorCfgFile;
    }

    @Override
    public File getIdmEngineConfigurationFile() {
        return this.idmEngineConfiguratorCfgFile;
    }

    @Override
    public void init() throws Exception {

        this.processEngineConfiguration = ProcessEngineConfiguration.createStandaloneProcessEngineConfiguration();
        this.processEngineConfiguration.setJdbcDriver(this.jdbcDriver);
        this.processEngineConfiguration.setJdbcUrl(this.jdbcUrl);
        this.processEngineConfiguration.setJdbcUsername(this.jdbcUsername).setJdbcPassword(this.jdbcPwd);
        this.processEngineConfiguration.setDatabaseSchemaUpdate("true");

        this.processEngineConfiguration.setAsyncExecutorActivate(false);
        this.processEngineConfiguration.setAsyncExecutor(null);

        assertInstanceOf(ProcessEngineConfigurationImpl.class, this.processEngineConfiguration);

        this.setIdmEngineConfiguratorCfgFile();

        ((ProcessEngineConfigurationImpl) this.processEngineConfiguration).setDisableIdmEngine(false);
        this.idmEngineConfigurator.setLogger(LOG);
        ((ProcessEngineConfigurationImpl) this.processEngineConfiguration)
                .setIdmEngineConfigurator(this.idmEngineConfigurator);
    }

    @Override
    public void setIdmEngineConfiguratorCfgFile(final File idmEngineConfiguratorCfgFile) throws IOException {
        this.idmEngineConfiguratorCfgFile = idmEngineConfiguratorCfgFile;
        this.setIdmEngineConfiguratorCfgFile();
    }

    private void setIdmEngineConfiguratorCfgFile() throws IOException {
        if (this.idmEngineConfiguratorCfgFile != null) {
            if (this.idmEngineConfiguratorCfgFile.getName()
                    .equals(FlowableClient.IDM_ENGINE_CONFIGURATION_TO_GENERATE)) {
                final Path idmEngineCfgDir = Files.createDirectories(this.rootFileSystem.resolve("idem-engine"));
                this.idmEngineConfiguratorCfgFile = Files.createFile(idmEngineCfgDir.resolve("idm-engine.cfg"))
                        .toFile();
                if (this.idmEngineConfigurator instanceof FileIdmEngineConfigurator) {
                    createFileIdmEngineConfigurationFiles(idmEngineCfgDir, this.idmEngineConfiguratorCfgFile);
                } else {
                    // For LDAP IDM engine, no other files is required.
                }
            }
        }
        this.idmEngineConfigurator.setConfigurationFile(this.idmEngineConfiguratorCfgFile);
    }

    @Override
    public void start() throws Exception {
        this.flowableClientEngine = this.processEngineConfiguration.buildProcessEngine();
    }

    private static void createFileIdmEngineConfigurationFiles(final Path idemEngineCfgDir,
            final File idmEngineConfiguratorCfgFile) throws IOException {

        final File usersConfigFile = Files.createFile(idemEngineCfgDir.resolve("usersConfigFile.properties")).toFile();
        final File groupsConfigFile = Files.createFile(idemEngineCfgDir.resolve("groupsConfigFile.properties"))
                .toFile();
        final File privilegesConfigFile = Files.createFile(idemEngineCfgDir.resolve("privilegesConfigFile.properties"))
                .toFile();

        final Properties baseConfig = new Properties();
        baseConfig.setProperty(FileIdmEngineConfigurator.PROP_USERS_FILE_NAME, usersConfigFile.getAbsolutePath());
        baseConfig.setProperty(FileIdmEngineConfigurator.PROP_GROUPS_FILE_NAME, groupsConfigFile.getAbsolutePath());
        baseConfig.setProperty(FileIdmEngineConfigurator.PROP_PRIVILEGES_FILE_NAME,
                privilegesConfigFile.getAbsolutePath());
        try (final OutputStream osBaseConfigFile = new FileOutputStream(idmEngineConfiguratorCfgFile)) {
            baseConfig.store(osBaseConfigFile, "");
        }
    }

    /**
     * Free the Flowable client, freeing the Flowable database if needed.
     */
    @Override
    public void close() throws Throwable {
        if (this.flowableClientEngine != null) {
            this.flowableClientEngine.close();
        }

        // Delete root file system of Flowable client
        if (this.rootFileSystemToClean) {
            Files.walk(this.rootFileSystem).sorted(Comparator.reverseOrder()).map(Path::toFile).forEach(File::delete);
        }
    }

    @Override
    public String getJdbcUrl() {
        return this.jdbcUrl;
    }

    @Override
    public ProcessEngine getProcessEngine() {
        return this.flowableClientEngine;
    }
}
